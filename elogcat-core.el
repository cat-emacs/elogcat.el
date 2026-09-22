;;; elogcat-core.el --- Shared data model for elogcat  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Youngjoo Lee

;;; Commentary:

;; Shared records and buffer-local state for elogcat modules.

;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defgroup elogcat nil
  "Android Logcat interface."
  :group 'external)

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
  "Hash table mapping PID strings to process metadata.")
(defvar-local elogcat--process-refresh-timer nil)
(defvar-local elogcat--process-refresh-process nil)
(defvar-local elogcat--package-refresh-process nil)
(defvar-local elogcat--package-refresh-callbacks nil)
(defvar-local elogcat--unresolved-records nil)
(defvar-local elogcat--package-message-cache nil)
(defvar-local elogcat-query-filter nil)
(defvar-local elogcat--query-predicate nil)
(defvar-local elogcat-query-match-case nil)
(defvar-local elogcat--query-completion-source-buffer nil)
(defvar-local elogcat--redraw-function #'ignore
  "Function called when a module needs the backlog redrawn.")
(defvar-local elogcat-device-serial nil
  "Serial of the Android device used by this Logcat buffer.")
(defvar-local elogcat-device-name nil)
(defvar-local elogcat--devices nil)
(defvar-local elogcat-stream-state 'stopped)
(defvar-local elogcat-stream-error nil)
(defvar-local elogcat--reconnect-timer nil)
(defvar-local elogcat--reconnect-attempt 0)
(defvar-local elogcat--intentional-stop nil)
(defvar-local elogcat--start-tail nil)
(defvar-local elogcat--device-query-process nil)
(defvar-local elogcat--stream-process nil)
(defvar-local elogcat--clear-process nil)
(defvar-local elogcat-project-root nil)
(defvar-local elogcat-query-error nil)
(defvar-local elogcat--collapsed-groups nil)
(defvar-local elogcat-visible-fields nil)
(defvar elogcat-query-filter-history nil)
(defconst elogcat-level-priority '("V" "D" "I" "W" "E" "F" "A"))

(defun elogcat--adb-command (&rest arguments)
  "Return an adb command for current device with ARGUMENTS."
  (append (list "adb")
          (when elogcat-device-serial
            (list "-s" elogcat-device-serial))
          arguments))

(defun elogcat--new-message-group (message)
  "Return a new message group initialized with MESSAGE."
  (cons message nil))

(defun elogcat--extend-message-group (group message)
  "Append MESSAGE to GROUP's aggregate text and return GROUP."
  (when group
    (setcar group (concat (car group) "\n" message)))
  group)

(defun elogcat--message-text (record)
  "Return RECORD's complete grouped message text."
  (or (car-safe (elogcat-record-message-group record))
      (elogcat-record-message record)))

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


(provide 'elogcat-core)
;;; elogcat-core.el ends here
