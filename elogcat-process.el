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

(defconst elogcat--process-query-command
  (concat "printf '__ELOGCAT_PACKAGES__\\n'; "
          "cmd package list packages -U; "
          "printf '__ELOGCAT_PROCESSES__\\n'; "
          "ps -A -n -o UID,PID,NAME 2>/dev/null || ps -A -o UID,PID,NAME")
  "Device shell command used to associate processes with packages.")

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

(defun elogcat--parse-process-query (output)
  "Return a PID table parsed from process metadata OUTPUT."
  (let ((uid-packages (make-hash-table :test #'equal))
        (table (make-hash-table :test #'equal))
        section)
    (dolist (line (split-string output "\n" t))
      (cond
       ((equal line "__ELOGCAT_PACKAGES__") (setq section 'packages))
       ((equal line "__ELOGCAT_PROCESSES__") (setq section 'processes))
       ((and (eq section 'packages)
             (string-match "^package:\\([^[:space:]]+\\).*uid:\\([0-9]+\\)" line))
        (push (match-string 1 line)
              (gethash (match-string 2 line) uid-packages)))
       ((and (eq section 'processes)
             (string-match
              "^[[:space:]]*\\([^[:space:]]+\\)[[:space:]]+\\([0-9]+\\)[[:space:]]+\\([^[:space:]]+\\)"
              line))
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
                   table)))))
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
  (let (changed)
    (dolist (record elogcat--unresolved-records)
      (when (gethash (elogcat-record-pid record) table)
        (elogcat--apply-process-info record table)
        (setq changed t)))
    (setq elogcat--unresolved-records nil)
    (when (and changed
               (or elogcat-package-filter elogcat--query-predicate))
      (funcall elogcat--redraw-function))))

(defun elogcat--process-query-sentinel (process _event)
  "Consume process metadata when PROCESS exits successfully."
  (let ((target (process-get process 'elogcat-target-buffer))
        (output-buffer (process-buffer process)))
    (unwind-protect
        (when (and (eq (process-status process) 'exit)
                   (= (process-exit-status process) 0)
                   (buffer-live-p target)
                   (buffer-live-p output-buffer))
          (let ((output (with-current-buffer output-buffer (buffer-string))))
            (with-current-buffer target
              (setq elogcat--process-refresh-process nil)
              (elogcat--update-process-table
               (elogcat--parse-process-query output)))))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))

(defun elogcat--refresh-process-table ()
  "Asynchronously refresh package and process metadata for this buffer."
  (unless (process-live-p elogcat--process-refresh-process)
    (let ((output-buffer (generate-new-buffer " *elogcat-processes*")))
      (setq elogcat--process-refresh-process
            (make-process
             :name "elogcat-processes"
             :buffer output-buffer
             :command (list "adb" "shell" elogcat--process-query-command)
             :connection-type 'pipe
             :noquery t
             :sentinel #'elogcat--process-query-sentinel))
      (process-put elogcat--process-refresh-process
                   'elogcat-target-buffer (current-buffer)))))

(defun elogcat--stop-process-monitor ()
  "Stop the current buffer's package process monitor."
  (when (timerp elogcat--process-refresh-timer)
    (cancel-timer elogcat--process-refresh-timer))
  (setq elogcat--process-refresh-timer nil)
  (when (process-live-p elogcat--process-refresh-process)
    (delete-process elogcat--process-refresh-process))
  (setq elogcat--process-refresh-process nil))

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
