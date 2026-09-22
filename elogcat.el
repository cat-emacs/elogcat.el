;;; elogcat.el --- logcat interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.3.0
;; Keywords: tools
;; Package-Requires: ((s "1.9.0") (dash "2.10.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; logcat interface for Emacs

;;; Code:
(require 's)
(require 'dash)
(require 'cl-lib)
(require 'seq)

;;;; Declarations
(cl-defstruct elogcat-record
  "One structured threadtime log record."
  raw level timestamp pid tid tag message application-ids process-name
  message-group system-p)

(cl-defstruct elogcat-process-info
  "Application metadata for one Android process."
  application-ids process-name)

(defvar-local elogcat-pending-output ""
  "Incomplete adb output waiting for its terminating newline.")

(defvar-local elogcat--records nil
  "Chronological backlog of `elogcat-record' values.")

(defvar-local elogcat--records-tail nil
  "Last cons cell of `elogcat--records' for constant-time appends.")

(defvar-local elogcat--backlog-size 0
  "Approximate character size of `elogcat--records'.")

(defvar-local elogcat-paused nil
  "Non-nil when rendering new Logcat records is paused.")

(defvar-local elogcat-follow-tail t
  "Non-nil when Logcat windows should follow new records.")

(defvar-local elogcat-package-filter nil
  "Package name used to filter the current Logcat buffer.")

(defvar-local elogcat--process-table nil
  "Hash table mapping PID strings to `elogcat-process-info' values.")

(defvar-local elogcat--process-refresh-timer nil
  "Timer used to refresh Android process metadata.")

(defvar-local elogcat--process-refresh-process nil
  "Active asynchronous process metadata query.")

(defvar-local elogcat--unresolved-records nil
  "Recent records awaiting one process metadata refresh.")

(defvar-local elogcat--package-message-cache nil
  "Message groups whose Error/Fatal/Assert text mentions the selected package.")

(defgroup elogcat nil
  "Interface with elogcat."
  :group 'external)

(defface elogcat-verbose-face '((t (:inherit default)))
  "Font Lock face used to highlight VERBOSE log records."
  :group 'elogcat)

(defface elogcat-debug-face '((t (:inherit font-lock-preprocessor-face)))
  "Font Lock face used to highlight DEBUG log records."
  :group 'elogcat)

(defface elogcat-info-face '((t (:inherit success)))
  "Font Lock face used to highlight INFO log records."
  :group 'elogcat)

(defface elogcat-warning-face '((t (:inherit warning)))
  "Font Lock face used to highlight WARN log records."
  :group 'elogcat)

(defface elogcat-error-face '((t (:inherit error)))
  "Font Lock face used to highlight ERROR log records."
  :group 'elogcat)

(defface elogcat-fatal-face '((t (:inherit error)))
  "Font Lock face used to highlight FATAL log records."
  :group 'elogcat)

(defvar elogcat-face-alist
  '(("V" . elogcat-verbose-face)
    ("D" . elogcat-debug-face)
    ("I" . elogcat-info-face)
    ("W" . elogcat-warning-face)
    ("E" . elogcat-error-face)
    ("F" . elogcat-fatal-face)
    ("A" . elogcat-fatal-face)))

(defconst elogcat-level-priority '("V" "D" "I" "W" "E" "F" "A")
  "Log levels in ascending priority order.")

(defcustom elogcat-logcat-command
  "logcat -v threadtime -b main -b system -b radio -b events -b crash -b kernel"
  "Logcat command."
  :group 'elogcat
  :type 'string)

(defcustom elogcat-default-tail 1
  "Default number of historical lines to show with `-T'.
Set to nil to replay the full ring buffer by default."
  :group 'elogcat
  :type '(choice (const :tag "Full history" nil)
                 (integer :tag "Number of lines")))

(defcustom elogcat-backlog-size (* 4 1024 1024)
  "Maximum approximate character size of the in-memory message backlog.
The backlog enables filtering and formatting without restarting adb."
  :group 'elogcat
  :type 'integer)

(defcustom elogcat-soft-wrap nil
  "Whether new Logcat buffers use visual line wrapping."
  :group 'elogcat
  :type 'boolean)

(defcustom elogcat-process-refresh-interval 2
  "Seconds between process metadata refreshes while filtering by package."
  :group 'elogcat
  :type 'number)

(defvar-local elogcat-include-filter-regexp nil)
(defvar-local elogcat-exclude-filter-regexp nil)
(defvar-local elogcat-min-level "V"
  "Minimum log level to display.  One of V D I W E F A.")

(defconst elogcat-process-name "elogcat")

(defcustom elogcat-buffer "*elogcat*"
  "Name for elogcat buffer."
  :group 'elogcat
  :type 'string)

(defcustom elogcat-mode-line '(:eval (elogcat-make-status))
  "Mode line lighter for elogcat."
  :group 'elogcat
  :type 'sexp
  :risky t
  :package-version '(elogcat . "0.2.0"))

(defun elogcat-get-log-buffer-status (buffer)
  "Get a log buffer status by BUFFER."
  (let ((end (if (string= buffer "kernel") "" "|")))
    (if (s-contains? buffer elogcat-logcat-command)
        (concat (s-word-initials buffer) end)
      (concat "-" end))))

(defun elogcat--at-tail-p ()
  "Return non-nil when the current buffer is displayed at its tail."
  (let ((windows (get-buffer-window-list (current-buffer) nil t))
        (tail (point-max)))
    (if windows
        (cl-some (lambda (window) (>= (window-point window) tail)) windows)
      (>= (point) tail))))

(defun elogcat-make-status (&optional _status)
  "Get a log buffer status for use in the mode line."
  (format " elogcat[%s]%s<%s> %s%s"
          (mapconcat #'elogcat-get-log-buffer-status
                     '("main" "system" "radio" "events" "crash" "kernel") "")
          (if elogcat-package-filter
              (format "(%s)" elogcat-package-filter)
            "")
          elogcat-min-level
          (if elogcat-paused
              "PAUSED"
            (if (and elogcat-follow-tail (elogcat--at-tail-p)) "LIVE" "HOLD"))
          (if truncate-lines "" " WRAP")))

(defun elogcat-erase-buffer ()
  "Clear elogcat buffer."
  (interactive)
  (with-current-buffer elogcat-buffer
    (let ((buffer-read-only nil))
      (erase-buffer)
      (setq elogcat--records nil
            elogcat--records-tail nil
            elogcat--unresolved-records nil
            elogcat--backlog-size 0
            elogcat-pending-output "")))
  (start-process-shell-command "elogcat-clear"
                               "*elogcat-clear*"
                               (concat "adb " elogcat-logcat-command " -c"))
  (sleep-for 1)
  (elogcat-stop)
  (elogcat))

(defun elogcat--redraw-unless-paused ()
  "Redraw the retained backlog unless display updates are paused."
  (unless elogcat-paused
    (elogcat--render-backlog))
  (force-mode-line-update))

(defun elogcat-clear-filter (filter)
  "Clear FILTER and redraw the retained message backlog."
  (set filter nil)
  (elogcat--redraw-unless-paused)
  (message "elogcat: %s cleared" filter))

(defun elogcat-clear-include-filter ()
  "Clear the include filter."
  (interactive)
  (elogcat-clear-filter 'elogcat-include-filter-regexp))

(defun elogcat-clear-exclude-filter ()
  "Clear the exclude filter."
  (interactive)
  (elogcat-clear-filter 'elogcat-exclude-filter-regexp))

(defun elogcat-set-filter (regexp filter)
  "Set FILTER to REGEXP and redraw the retained message backlog."
  (set filter (unless (string-empty-p regexp) regexp))
  (elogcat--redraw-unless-paused)
  (message "elogcat: %s set to %S" filter (symbol-value filter)))

(defun elogcat-show-status ()
  "Show current Logcat state in the echo area."
  (interactive)
  (message
   "elogcat: %s; package=%S (%d processes); include=%S; exclude=%S; level=%s; %d records"
   (if elogcat-paused "paused" (if elogcat-follow-tail "live" "hold"))
   elogcat-package-filter
   (if (hash-table-p elogcat--process-table)
       (hash-table-count elogcat--process-table)
     0)
   elogcat-include-filter-regexp elogcat-exclude-filter-regexp
   elogcat-min-level (length elogcat--records)))

(defun elogcat-set-include-filter (regexp)
  "Set the REGEXP for include filter."
  (interactive "MRegexp Include Filter: ")
  (elogcat-set-filter regexp 'elogcat-include-filter-regexp))

(defun elogcat-set-exclude-filter (regexp)
  "Set the REGEXP for exclude filter."
  (interactive "MRegexp Exclude Filter: ")
  (elogcat-set-filter regexp 'elogcat-exclude-filter-regexp))

(defun elogcat-set-level (level)
  "Set minimum log LEVEL filter.
Only lines at or above this level will be displayed."
  (interactive
   (list (completing-read
          (format "Min level (current: %s): " elogcat-min-level)
          '("V - Verbose" "D - Debug" "I - Info"
            "W - Warning" "E - Error" "F - Fatal" "A - Assert")
          nil t)))
  (setq elogcat-min-level (substring level 0 1))
  (elogcat--redraw-unless-paused)
  (message "elogcat: min level set to %s" elogcat-min-level))

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
  "Disambiguate PACKAGES using PROCESS-NAME when possible."
  (or (seq-filter
       (lambda (package)
         (or (equal process-name package)
             (string-prefix-p (concat package ":") process-name)))
       packages)
      packages))

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
    (when (and changed elogcat-package-filter)
      (elogcat--redraw-unless-paused))))

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

(defconst elogcat--threadtime-regexp
  (concat "^\\([0-9][0-9]-[0-9][0-9] +[0-9:.]+\\)"
          " +\\([0-9]+\\) +\\([0-9]+\\) \\([VDIWEFA]\\) "
          "\\([^:]*?\\) *: \\(.*\\)$")
  "Regexp for adb logcat's threadtime format.")

(defun elogcat--track-unresolved-record (record)
  "Queue RECORD for the next process metadata refresh when needed."
  (when (and (elogcat-record-pid record)
             (null (elogcat-record-application-ids record)))
    (push record elogcat--unresolved-records))
  record)

(defun elogcat--same-message-header-p (record previous)
  "Return non-nil when RECORD and PREVIOUS share one threadtime header."
  (and previous
       (equal (elogcat-record-timestamp record)
              (elogcat-record-timestamp previous))
       (equal (elogcat-record-pid record) (elogcat-record-pid previous))
       (equal (elogcat-record-tid record) (elogcat-record-tid previous))
       (equal (elogcat-record-level record) (elogcat-record-level previous))
       (equal (elogcat-record-tag record) (elogcat-record-tag previous))))

(defun elogcat--parse-record (line &optional previous)
  "Parse threadtime LINE, inheriting metadata from PREVIOUS for continuations."
  (cond
   ((string-prefix-p "--------- beginning of " line)
    (make-elogcat-record
     :raw line :message line :message-group (cons nil nil) :system-p t))
   ((string-match elogcat--threadtime-regexp line)
      (let ((record
             (make-elogcat-record
              :raw line :timestamp (match-string 1 line)
              :pid (match-string 2 line) :tid (match-string 3 line)
              :level (match-string 4 line)
              :tag (string-trim (match-string 5 line))
              :message (match-string 6 line))))
        (setf (elogcat-record-message-group record)
              (if (elogcat--same-message-header-p record previous)
                  (elogcat-record-message-group previous)
                (cons nil nil)))
        (elogcat--apply-process-info record)
        (elogcat--track-unresolved-record record)))
   (t
    (elogcat--track-unresolved-record
     (make-elogcat-record
      :raw line
      :timestamp (and previous (elogcat-record-timestamp previous))
      :pid (and previous (elogcat-record-pid previous))
      :tid (and previous (elogcat-record-tid previous))
      :level (and previous (elogcat-record-level previous))
      :tag (and previous (elogcat-record-tag previous))
      :application-ids (and previous (elogcat-record-application-ids previous))
      :process-name (and previous (elogcat-record-process-name previous))
      :message-group (or (and previous (elogcat-record-message-group previous))
                         (cons nil nil))
      :message line)))))

(defun elogcat--record-filter-text (record)
  "Return searchable text for RECORD, including inherited metadata."
  (mapconcat #'identity
             (delq nil (list (elogcat-record-raw record)
                             (elogcat-record-timestamp record)
                             (elogcat-record-pid record)
                             (elogcat-record-tid record)
                             (elogcat-record-level record)
                             (elogcat-record-tag record)
                             (elogcat-record-process-name record)
                             (string-join (elogcat-record-application-ids record) " ")
                             (elogcat-record-message record)))
             " "))

(defun elogcat--rebuild-package-message-cache (&optional records)
  "Cache Error/Fatal/Assert groups mentioning the selected package in RECORDS."
  (setq elogcat--package-message-cache (make-hash-table :test #'eq))
  (when elogcat-package-filter
    (dolist (record (or records elogcat--records))
      (when (and (member (elogcat-record-level record) '("E" "F" "A"))
                 (string-match-p (regexp-quote elogcat-package-filter)
                                 (elogcat-record-message record)))
        (puthash (elogcat-record-message-group record) t
                 elogcat--package-message-cache)))))

(defun elogcat--cache-package-message-groups (records)
  "Cache matching groups from RECORDS and return non-nil for a new match."
  (let (changed)
    (when elogcat-package-filter
      (unless (hash-table-p elogcat--package-message-cache)
        (setq elogcat--package-message-cache (make-hash-table :test #'eq)))
      (dolist (record records)
        (let ((group (elogcat-record-message-group record)))
          (when (and (member (elogcat-record-level record) '("E" "F" "A"))
                     (string-match-p (regexp-quote elogcat-package-filter)
                                     (elogcat-record-message record))
                     (not (gethash group elogcat--package-message-cache)))
            (puthash group t elogcat--package-message-cache)
            (setq changed t)))))
    changed))

(defun elogcat--package-matches-p (record)
  "Return non-nil when RECORD belongs to `elogcat-package-filter'."
  (or (null elogcat-package-filter)
      (elogcat-record-system-p record)
      (member elogcat-package-filter (elogcat-record-application-ids record))
      (and (member (elogcat-record-level record) '("E" "F" "A"))
           (gethash (elogcat-record-message-group record)
                    elogcat--package-message-cache))))

(defun elogcat--record-matches-p (record)
  "Return non-nil when RECORD passes current filters."
  (if (elogcat-record-system-p record)
      t
    (let* ((text (elogcat--record-filter-text record))
           (level (elogcat-record-level record))
           (minimum (or (cl-position elogcat-min-level elogcat-level-priority
                                     :test #'string=) 0)))
      (and (elogcat--package-matches-p record)
           (or (null level)
               (>= (or (cl-position level elogcat-level-priority
                                    :test #'string=) 0)
                   minimum))
           (or (null elogcat-include-filter-regexp)
               (string-match-p elogcat-include-filter-regexp text))
           (or (null elogcat-exclude-filter-regexp)
               (not (string-match-p elogcat-exclude-filter-regexp text)))))))

(defun elogcat--record-size (record)
  "Return approximate retained size of RECORD."
  (1+ (length (elogcat-record-raw record))))

(defun elogcat--add-records (records)
  "Append RECORDS, trim the backlog, and return non-nil when trimmed."
  (when records
    (let ((new-tail (last records)))
      (if elogcat--records
          (progn
            (unless elogcat--records-tail
              (setq elogcat--records-tail (last elogcat--records)))
            (setcdr elogcat--records-tail records))
        (setq elogcat--records records))
      (setq elogcat--records-tail new-tail)))
  (cl-incf elogcat--backlog-size
           (cl-loop for record in records sum (elogcat--record-size record)))
  (let (trimmed)
    (while (and elogcat--records
                (> elogcat--backlog-size elogcat-backlog-size))
      (setq trimmed t)
      (cl-decf elogcat--backlog-size
               (elogcat--record-size (pop elogcat--records))))
    (unless elogcat--records
      (setq elogcat--records-tail nil))
    trimmed))

(defun elogcat--format-record (record)
  "Return RECORD formatted for insertion into the Logcat buffer."
  (let* ((level (elogcat-record-level record))
         (face (cdr (or (assoc level elogcat-face-alist)
                        (assoc "V" elogcat-face-alist))))
         (occurrence (and (or (and (member level '("E" "F" "A"))
                                   (string-match-p elogcat--threadtime-regexp
                                                   (elogcat-record-raw record)))
                              (string-match-p
                               (concat "^[[:space:]]*\\(?:at .+(.*:[0-9]+)"
                                       "\\|Caused by:\\|Suppressed:\\)")
                               (elogcat-record-message record)))
                          record)))
    (propertize (concat (elogcat-record-raw record) "\n")
                'face face
                'elogcat-record record
                'elogcat-occurrence occurrence
                'rear-nonsticky t)))

(defun elogcat--insert-records (records)
  "Insert matching RECORDS at the end of the current Logcat buffer."
  (let ((chunks (delq nil
                      (mapcar (lambda (record)
                                (when (elogcat--record-matches-p record)
                                  (elogcat--format-record record)))
                              records))))
    (when chunks
      (save-excursion
        (goto-char (point-max))
        (insert (apply #'concat chunks))))))

(defun elogcat--record-at-position (position)
  "Return the Logcat record at POSITION, including a final newline boundary."
  (or (get-text-property position 'elogcat-record)
      (and (> position (point-min))
           (get-text-property (1- position) 'elogcat-record))))

(defun elogcat--record-position (record)
  "Return the display position for RECORD, or nil when it is filtered out."
  (let ((position (point-min)) found)
    (while (and (< position (point-max)) (not found))
      (if (eq (get-text-property position 'elogcat-record) record)
          (setq found position)
        (setq position (or (next-single-property-change
                            position 'elogcat-record nil (point-max))
                           (point-max)))))
    found))

(defun elogcat--render-backlog (&optional force-tail)
  "Redraw from the backlog; with FORCE-TAIL, move followed windows to the end."
  (when (bound-and-true-p elogcat-mode)
    (let* ((buffer-read-only nil)
           (inhibit-redisplay t)
           (windows (get-buffer-window-list (current-buffer) nil t))
           (old-max (point-max))
           (point-record (elogcat--record-at-position (point)))
           (old-point (point))
           (window-records
            (mapcar (lambda (window)
                      (list window
                            (elogcat--record-at-position (window-point window))
                            (>= (window-point window) old-max)))
                    windows)))
      (erase-buffer)
      (elogcat--insert-records elogcat--records)
      (if (and elogcat-follow-tail
               (or force-tail (>= old-point old-max)))
          (goto-char (point-max))
        (goto-char (or (elogcat--record-position point-record)
                       (min old-point (point-max)))))
      (dolist (entry window-records)
        (set-window-point
         (car entry)
         (if (and elogcat-follow-tail (or force-tail (nth 2 entry)))
             (point-max)
           (or (elogcat--record-position (nth 1 entry))
               (min (window-point (car entry)) (point-max)))))))))

(defun elogcat--consume-output (output)
  "Parse complete records from OUTPUT and retain an incomplete suffix."
  (let* ((raw (concat elogcat-pending-output output))
         (text (if (string-search "\r" raw) (string-replace "\r" "" raw) raw))
         (position 0)
         (previous (and elogcat--records-tail
                        (car elogcat--records-tail)))
         records)
    (while (string-match "\n" text position)
      (let* ((line-end (match-beginning 0))
             (next-position (match-end 0))
             (record (elogcat--parse-record
                      (substring text position line-end) previous)))
        (push record records)
        (setq previous record
              position next-position)))
    (setq elogcat-pending-output (substring text position))
    (nreverse records)))

(defun elogcat-process-filter (_process output)
  "Retain and display structured Logcat records parsed from OUTPUT."
  (when-let* ((buffer (get-buffer elogcat-buffer)))
    (with-current-buffer buffer
      (let* ((old-max (point-max))
             (following-windows
              (and elogcat-follow-tail
                   (cl-loop for window in (get-buffer-window-list buffer nil t)
                            when (>= (window-point window) old-max)
                            collect window)))
             (records (elogcat--consume-output output))
             (gc-cons-threshold most-positive-fixnum)
             (inhibit-redisplay t)
             (buffer-read-only nil)
             (trimmed (elogcat--add-records records))
             (new-package-group
              (if trimmed
                  (progn
                    (elogcat--rebuild-package-message-cache)
                    t)
                (elogcat--cache-package-message-groups records))))
        (unless elogcat-paused
          (if (or trimmed new-package-group)
              (elogcat--render-backlog)
            (elogcat--insert-records records))
          (dolist (window following-windows)
            (set-window-point window (point-max))))))))


(defun elogcat-process-sentinel (_process _event)
  "Update Logcat status after the adb process changes state."
  (force-mode-line-update t))

(defun elogcat-toggle-pause ()
  "Pause or resume rendering while continuing to retain incoming records."
  (interactive)
  (setq elogcat-paused (not elogcat-paused))
  (unless elogcat-paused
    (elogcat--render-backlog t))
  (force-mode-line-update)
  (message "elogcat: %s" (if elogcat-paused "paused" "resumed")))

(defun elogcat-toggle-follow-tail ()
  "Toggle automatic scrolling to newly appended Logcat records."
  (interactive)
  (setq elogcat-follow-tail (not elogcat-follow-tail))
  (when elogcat-follow-tail
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-point window (point-max)))
    (goto-char (point-max)))
  (force-mode-line-update)
  (message "elogcat: follow tail %s" (if elogcat-follow-tail "on" "off")))

(defun elogcat-toggle-soft-wrap ()
  "Toggle visual line wrapping in the current Logcat buffer."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (force-mode-line-update)
  (message "elogcat: soft wrap %s" (if truncate-lines "off" "on")))

(defun elogcat--occurrence-positions ()
  "Return positions of errors, assertions, and stack frames in this buffer."
  (let ((position (point-min)) positions)
    (while (< position (point-max))
      (when (get-text-property position 'elogcat-occurrence)
        (push position positions))
      (setq position (or (next-single-property-change
                          position 'elogcat-occurrence nil (point-max))
                         (point-max))))
    (nreverse positions)))

(defun elogcat--move-occurrence (step)
  "Move STEP occurrences, wrapping at buffer boundaries."
  (let ((positions (elogcat--occurrence-positions)))
    (unless positions
      (user-error "No errors or stack frames in the Logcat buffer"))
    (let ((target
           (if (> step 0)
               (or (seq-find (lambda (position) (> position (point))) positions)
                   (car positions))
             (or (car (last (seq-take-while
                             (lambda (position) (< position (point)))
                             positions)))
                 (car (last positions))))))
      (goto-char target)
      (beginning-of-line)
      (when (get-buffer-window (current-buffer) t)
        (recenter)))))

(defun elogcat-next-occurrence ()
  "Move to the next error, assertion, or stack frame, wrapping at the end."
  (interactive)
  (elogcat--move-occurrence 1))

(defun elogcat-previous-occurrence ()
  "Move to the previous error, assertion, or stack frame, wrapping at the start."
  (interactive)
  (elogcat--move-occurrence -1))

(defmacro elogcat-define-toggle-function (sym ring-buffer-name)
  "Define a function with SYM and RING-BUFFER-NAME."
  (let ((fun (intern (format "elogcat-toggle-%s" sym)))
        (doc (format "Switch to %s" ring-buffer-name)))
    `(progn
       (defun ,fun () ,doc
              (interactive)
              (let ((option (concat "-b " ,ring-buffer-name)))
                (if (s-contains? option elogcat-logcat-command)
                    (setq elogcat-logcat-command
                          (mapconcat (lambda (args) (concat (s-trim args)))
                                     (s-split option elogcat-logcat-command) " "))
                  (setq elogcat-logcat-command
                        (s-concat (s-trim elogcat-logcat-command) " " option))))
              (let ((buffer-read-only nil))
                (erase-buffer)
                (setq elogcat--records nil
                      elogcat--records-tail nil
                      elogcat--unresolved-records nil
                      elogcat--backlog-size 0
                      elogcat-pending-output ""))
              (elogcat-stop)
              (elogcat)))))

(elogcat-define-toggle-function main "main")
(elogcat-define-toggle-function system "system")
(elogcat-define-toggle-function radio "radio")
(elogcat-define-toggle-function events "events")
(elogcat-define-toggle-function crash "crash")
(elogcat-define-toggle-function kernel "kernel")

(defvar elogcat-mode-map nil
  "Keymap for elogcat minor mode.")

(unless elogcat-mode-map
  (setq elogcat-mode-map (make-sparse-keymap)))

(--each '(("C" . elogcat-erase-buffer)
          ("SPC" . elogcat-toggle-pause)
          ("f" . elogcat-toggle-follow-tail)
          ("W" . elogcat-toggle-soft-wrap)
          ("n" . elogcat-next-occurrence)
          ("p" . elogcat-previous-occurrence)
          ("i" . elogcat-set-include-filter)
          ("x" . elogcat-set-exclude-filter)
          ("I" . elogcat-clear-include-filter)
          ("X" . elogcat-clear-exclude-filter)
          ("L" . elogcat-set-level)
          ("P" . elogcat-toggle-package)
          ("S" . elogcat-save-buffer)
          ("g" . elogcat-show-status)
          ("F" . occur)
          ("q" . elogcat-exit)
          ("m" . elogcat-toggle-main)
          ("s" . elogcat-toggle-system)
          ("r" . elogcat-toggle-radio)
          ("e" . elogcat-toggle-events)
          ("c" . elogcat-toggle-crash)
          ("k" . elogcat-toggle-kernel))
  (define-key elogcat-mode-map (read-kbd-macro (car it)) (cdr it)))

(define-minor-mode elogcat-mode
  "Minor mode for browsing a structured Android Logcat stream."
  :lighter elogcat-mode-line
  :keymap elogcat-mode-map
  (when elogcat-mode
    (setq-local truncate-lines (not elogcat-soft-wrap)
                elogcat-paused nil
                elogcat-follow-tail t
                elogcat--process-table (make-hash-table :test #'equal)
                elogcat--unresolved-records nil
                elogcat--package-message-cache (make-hash-table :test #'eq))
    (add-hook 'kill-buffer-hook #'elogcat--stop-process-monitor nil t)
    (buffer-disable-undo)))

(defun elogcat-exit ()
  "Exit elogcat."
  (interactive)
  (let* ((buf (current-buffer))
         (proc (get-buffer-process buf)))
    (when (process-live-p proc)
      (kill-process proc)
      (sleep-for 0.1))
    (kill-buffer buf)))

(defun elogcat-toggle-package (package)
  "Toggle local structured filtering by Android application PACKAGE.
The Logcat process and retained backlog are not restarted or cleared."
  (interactive
   (list (completing-read
          "Filter package (select current package again to clear): "
          (mapcar (lambda (name)
                    (s-chop-prefix "package:" name))
                  (split-string
                   (string-trim
                    (shell-command-to-string
                     "adb shell pm list packages -3"))
                   "\n" t))
          nil nil nil nil elogcat-package-filter)))
  (setq elogcat-package-filter
        (unless (or (string-empty-p package)
                    (equal package elogcat-package-filter))
          package))
  (elogcat--rebuild-package-message-cache)
  (elogcat--refresh-process-table)
  (elogcat--redraw-unless-paused)
  (message "elogcat: package filter %s"
           (or elogcat-package-filter "cleared")))

(defun elogcat-stop ()
  "Stop the adb Logcat process and package process monitor."
  (when (bound-and-true-p elogcat-mode)
    (elogcat--stop-process-monitor))
  (-when-let (proc (get-process "elogcat"))
    (delete-process proc)))

;;;###autoload
(defun elogcat (&optional arg)
  "Start the adb logcat process.
Without prefix, show the last `elogcat-default-tail' lines then stream.
With numeric prefix N, show the last N lines then stream.
With bare \\[universal-argument], replay full ring buffer history."
  (interactive "P")
  (unless (get-process "elogcat")
    (let* ((tail-arg (cond
                      ((consp arg) "")
                      (arg (format " -T %d" (prefix-numeric-value arg)))
                      (elogcat-default-tail
                       (format " -T %d" elogcat-default-tail))
                      (t "")))
           (cmd (concat elogcat-logcat-command
                        (unless (s-contains? "-b" elogcat-logcat-command)
                          " -s")
                        tail-arg))
           (proc (start-process-shell-command
                  "elogcat" elogcat-buffer
                  (concat "adb shell " (shell-quote-argument cmd)))))
      (set-process-filter proc #'elogcat-process-filter)
      (set-process-sentinel proc #'elogcat-process-sentinel)
      (with-current-buffer elogcat-buffer
        (elogcat-mode t)
        (setq buffer-read-only t)
        (font-lock-mode -1)
        (elogcat--start-process-monitor))
      (switch-to-buffer elogcat-buffer)
      (goto-char (point-max)))))

(defun elogcat-save-buffer ()
  "Save the current elogcat buffer to a file and stop the logcat process."
  (interactive)
  (save-buffer)
  (elogcat-stop))

(provide 'elogcat)
;;; elogcat.el ends here
