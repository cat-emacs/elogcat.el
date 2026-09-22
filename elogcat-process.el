;;; elogcat-process.el --- Process metadata for elogcat  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Youngjoo Lee

;;; Commentary:

;; Asynchronous Android PID, process name, and application ID monitoring.

;;; Code:
(require 'seq)
(require 'elogcat-core)

(defcustom elogcat-process-refresh-interval 2
  "Seconds between process metadata refreshes."
  :group 'elogcat
  :type 'number)

(defcustom elogcat-package-cache-ttl 60
  "Seconds to cache installed package UID mappings per device."
  :group 'elogcat
  :type 'number)

(defvar elogcat--package-cache (make-hash-table :test #'equal)
  "Device serial to cached package metadata.")

(defconst elogcat--package-query-command
  "cmd package list packages -U"
  "Device command used to read package UID mappings.")

(defconst elogcat--process-list-command
  "ps -A -n -o UID,PID,NAME 2>/dev/null || ps -A -o UID,PID,NAME"
  "Device command used to read running processes.")

(defun elogcat--numeric-android-uid (uid)
  "Return UID normalized from a numeric or Android uN_aM process user."
  (cond
   ((string-match-p "\\`[0-9]+\\'" uid) uid)
   ((string-match "\\`u\\([0-9]+\\)_a\\([0-9]+\\)\\'" uid)
    (number-to-string
     (+ (* (string-to-number (match-string 1 uid)) 100000)
        10000
        (string-to-number (match-string 2 uid)))))
   (t uid)))

(defun elogcat--packages-for-process (packages process-name)
  "Disambiguate PACKAGES using PROCESS-NAME without false shared-UID matches."
  (let ((exact (member process-name packages))
        (prefixed
         (seq-filter
          (lambda (package)
            (string-prefix-p (concat package ":") process-name))
          packages)))
    (cond
     (exact (list process-name))
     (prefixed prefixed)
     ((= (length packages) 1) packages)
     (t nil))))

(defun elogcat--package-cache-key (&optional serial)
  "Return the package-cache key for SERIAL or the current device."
  (or serial elogcat-device-serial "default"))

(defun elogcat--parse-package-output (output)
  "Return UID and package metadata parsed from OUTPUT."
  (let ((uid-packages (make-hash-table :test #'equal)) packages)
    (dolist (line (split-string output "\n" t))
      (when (string-match
             "^package:\\([^[:space:]]+\\).*uid:\\([0-9]+\\)" line)
        (let ((package (match-string 1 line))
              (uid (match-string 2 line)))
          (push package (gethash uid uid-packages))
          (push package packages))))
    (list :time (float-time) :uids uid-packages
          :packages (delete-dups packages))))

(defun elogcat--parse-process-output (output uid-packages)
  "Return a PID table parsed from OUTPUT using UID-PACKAGES."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (line (split-string output "\n" t))
      (when (string-match
             "^[[:space:]]*\\([^[:space:]]+\\)[[:space:]]+\\([0-9]+\\)[[:space:]]+\\([^[:space:]]+\\)"
             line)
        (let* ((uid (match-string 1 line))
               (pid (match-string 2 line))
               (process-name (match-string 3 line))
               (packages
                (elogcat--packages-for-process
                 (gethash (elogcat--numeric-android-uid uid) uid-packages)
                 process-name)))
          (puthash pid
                   (make-elogcat-process-info
                    :application-ids packages
                    :process-name process-name)
                   table))))
    table))

(defun elogcat--apply-process-info (record &optional table)
  "Apply process metadata from TABLE to RECORD and return RECORD."
  (let ((process-table (or table elogcat--process-table)))
    (when (hash-table-p process-table)
      (when-let* ((info (gethash (elogcat-record-pid record) process-table)))
        (setf (elogcat-record-application-ids record)
              (elogcat-process-info-application-ids info)
              (elogcat-record-process-name record)
              (elogcat-process-info-process-name info)))))
  record)

(defun elogcat--update-process-table (table)
  "Install TABLE and enrich recently unresolved records."
  (setq elogcat--process-table table)
  (let (changed unresolved)
    (dolist (record elogcat--unresolved-records)
      (if (gethash (elogcat-record-pid record) table)
          (progn
            (elogcat--apply-process-info record table)
            (setq changed t))
        (push record unresolved)))
    (setq elogcat--unresolved-records (nreverse unresolved))
    (when (and changed
               (or elogcat-package-filter elogcat--query-predicate))
      (funcall elogcat--redraw-function))))

(defun elogcat--cached-package-metadata (&optional allow-stale)
  "Return package metadata for current device, optionally ALLOW-STALE."
  (when-let* ((metadata (gethash (elogcat--package-cache-key)
                                 elogcat--package-cache)))
    (when (or allow-stale
              (< (- (float-time) (plist-get metadata :time))
                 elogcat-package-cache-ttl))
      metadata)))

(defun elogcat--finish-package-query (process output-buffer)
  "Consume package query PROCESS output from OUTPUT-BUFFER."
  (when (eq process elogcat--package-refresh-process)
    (setq elogcat--package-refresh-process nil)
    (let ((serial (process-get process 'elogcat-device-serial))
          (metadata nil))
      (when (and (equal serial elogcat-device-serial)
                 (eq (process-status process) 'exit)
                 (= (process-exit-status process) 0)
                 (buffer-live-p output-buffer))
        (setq metadata
              (elogcat--parse-package-output
               (with-current-buffer output-buffer (buffer-string))))
        (puthash (elogcat--package-cache-key serial) metadata
                 elogcat--package-cache))
      (let ((callbacks (nreverse elogcat--package-refresh-callbacks)))
        (setq elogcat--package-refresh-callbacks nil)
        (dolist (callback callbacks)
          (funcall callback metadata))))))

(defun elogcat--package-query-sentinel (process _event)
  "Cache package metadata produced by PROCESS and run waiting callbacks."
  (let ((target (process-get process 'elogcat-target-buffer))
        (output-buffer (process-buffer process)))
    (unwind-protect
        (when (buffer-live-p target)
          (with-current-buffer target
            (elogcat--finish-package-query process output-buffer)))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))

(defun elogcat--ensure-package-metadata (callback &optional force)
  "Call CALLBACK with package metadata, refreshing asynchronously if needed.
With FORCE, ignore a fresh cache entry."
  (if-let* ((metadata (and (not force) (elogcat--cached-package-metadata))))
      (funcall callback metadata)
    (push callback elogcat--package-refresh-callbacks)
    (unless (process-live-p elogcat--package-refresh-process)
      (let ((output-buffer (generate-new-buffer " *elogcat-packages*")))
        (setq elogcat--package-refresh-process
              (make-process
               :name "elogcat-packages"
               :buffer output-buffer
               :command (elogcat--adb-command
                         "shell" elogcat--package-query-command)
               :connection-type 'pipe :noquery t
               :sentinel #'elogcat--package-query-sentinel))
        (process-put elogcat--package-refresh-process
                     'elogcat-target-buffer (current-buffer))
        (process-put elogcat--package-refresh-process
                     'elogcat-device-serial elogcat-device-serial)))))

(defun elogcat--finish-process-query (process output-buffer)
  "Consume process query PROCESS output from OUTPUT-BUFFER."
  (when (eq process elogcat--process-refresh-process)
    (setq elogcat--process-refresh-process nil)
    (let ((serial (process-get process 'elogcat-device-serial)))
      (when (and (equal serial elogcat-device-serial)
                 (eq (process-status process) 'exit)
                 (= (process-exit-status process) 0)
                 (buffer-live-p output-buffer))
        (when-let* ((metadata
                     (gethash (elogcat--package-cache-key serial)
                              elogcat--package-cache)))
          (elogcat--update-process-table
           (elogcat--parse-process-output
            (with-current-buffer output-buffer (buffer-string))
            (plist-get metadata :uids))))))))

(defun elogcat--process-query-sentinel (process _event)
  "Consume process metadata when PROCESS exits successfully."
  (let ((target (process-get process 'elogcat-target-buffer))
        (output-buffer (process-buffer process)))
    (unwind-protect
        (when (buffer-live-p target)
          (with-current-buffer target
            (elogcat--finish-process-query process output-buffer)))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))

(defun elogcat--start-process-query ()
  "Start one asynchronous Android process query for the current device."
  (unless (process-live-p elogcat--process-refresh-process)
    (let ((output-buffer (generate-new-buffer " *elogcat-processes*")))
      (setq elogcat--process-refresh-process
            (make-process
             :name "elogcat-processes" :buffer output-buffer
             :command (elogcat--adb-command
                       "shell" elogcat--process-list-command)
             :connection-type 'pipe :noquery t
             :sentinel #'elogcat--process-query-sentinel))
      (process-put elogcat--process-refresh-process
                   'elogcat-target-buffer (current-buffer))
      (process-put elogcat--process-refresh-process
                   'elogcat-device-serial elogcat-device-serial))))

(defun elogcat--refresh-process-table ()
  "Asynchronously refresh package metadata when needed, then process metadata."
  (elogcat--ensure-package-metadata
   (lambda (metadata)
     (when metadata (elogcat--start-process-query)))))

(defun elogcat--stop-process-monitor ()
  "Stop the current buffer's package process monitor."
  (when (timerp elogcat--process-refresh-timer)
    (cancel-timer elogcat--process-refresh-timer))
  (setq elogcat--process-refresh-timer nil)
  (when (process-live-p elogcat--process-refresh-process)
    (delete-process elogcat--process-refresh-process))
  (setq elogcat--package-refresh-callbacks nil)
  (when (process-live-p elogcat--package-refresh-process)
    (delete-process elogcat--package-refresh-process))
  (setq elogcat--process-refresh-process nil
        elogcat--package-refresh-process nil
        elogcat--package-refresh-callbacks nil))

(defun elogcat--start-process-monitor ()
  "Start refreshing package process metadata for this buffer."
  (elogcat--stop-process-monitor)
  (elogcat--refresh-process-table)
  (setq elogcat--process-refresh-timer
        (run-at-time elogcat-process-refresh-interval
                     elogcat-process-refresh-interval
                     (lambda (buffer)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (elogcat--refresh-process-table))))
                     (current-buffer))))


(provide 'elogcat-process)
;;; elogcat-process.el ends here
