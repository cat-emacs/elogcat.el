;;; elogcat.el --- logcat interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.3.0
;; Keywords: tools
;; Package-Requires: ((s "1.9.0") (dash "2.10.0") (transient "0.3.0"))

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
(require 'project)
(require 'transient)
(require 'elogcat-core)
(require 'elogcat-filter)
(require 'elogcat-process)

;;;; Declarations
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

(defcustom elogcat-default-query "package:mine"
  "Structured query used when a new Logcat session starts.
The default matches Android Studio.  Set this to nil to show all messages."
  :group 'elogcat
  :type '(choice (const :tag "Show all messages" nil) string))

(defcustom elogcat-project-package-function
  #'elogcat--android-mode-project-package
  "Function returning the application ID represented by `package:mine'.
It is called in the buffer from which `elogcat' starts.  The default uses
android-mode's current module and variant when available."
  :group 'elogcat
  :type '(choice (const :tag "Do not resolve automatically" nil) function))

(defcustom elogcat-show-key-hints t
  "Whether the Logcat header shows common key bindings."
  :group 'elogcat
  :type 'boolean)

(defcustom elogcat-default-device-serial nil
  "Preferred adb device serial, or nil to discover connected devices."
  :group 'elogcat
  :type '(choice (const :tag "Discover automatically" nil) string))

(defcustom elogcat-auto-reconnect-attempts 5
  "Maximum automatic reconnect attempts after an unexpected disconnect."
  :group 'elogcat
  :type 'integer)

(defcustom elogcat-default-visible-fields '(raw)
  "Fields shown for each record in a new Logcat buffer."
  :group 'elogcat
  :type '(set (const raw) (const timestamp) (const pid-tid)
              (const application) (const process) (const level)
              (const tag) (const message)))

(defcustom elogcat-saved-filters nil
  "Named Android Studio-compatible filter queries."
  :group 'elogcat
  :type '(alist :key-type string :value-type string))

(defvar-local elogcat-min-level "V"
  "Minimum log level to display.  One of V D I W E F A.")

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

(defun elogcat--stream-status-label ()
  "Return the current stream state as a display label."
  (upcase (symbol-name elogcat-stream-state)))

(defun elogcat--make-header-line ()
  "Return a concise Android Studio-style Logcat header."
  (concat
   " " (propertize "Device:" 'face 'bold)
   " " (or elogcat-device-name elogcat-device-serial "discovering")
   "    " (propertize "Mine:" 'face 'bold)
   " " (or elogcat-package-filter "not selected")
   "    " (propertize "Filter:" 'face 'bold)
   " " (or elogcat-query-filter "all messages")
   (when elogcat-query-error
     (propertize (concat "  ! " elogcat-query-error) 'face 'error))
   (when elogcat-show-key-hints
     (propertize "    ? menu   / filter   SPC pause   n/p errors"
                 'face 'shadow))))

(defun elogcat--at-tail-p ()
  "Return non-nil when the current buffer is displayed at its tail."
  (let ((windows (get-buffer-window-list (current-buffer) nil t))
        (tail (point-max)))
    (if windows
        (cl-some (lambda (window) (>= (window-point window) tail)) windows)
      (>= (point) tail))))

(defun elogcat-make-status (&optional _status)
  "Return concise stream state for use in the mode line."
  (format " elogcat %s · %s%s%s"
          (if elogcat-paused
              "PAUSED"
            (if (eq elogcat-stream-state 'live)
                (if (and elogcat-follow-tail (elogcat--at-tail-p))
                    "LIVE" "HOLD")
              (elogcat--stream-status-label)))
          (if (equal elogcat-min-level "V")
              "ALL"
            (concat elogcat-min-level "+"))
          (if truncate-lines "" " · WRAP")
          (if elogcat-query-match-case " · CASE" "")))

(defun elogcat--cancel-device-query ()
  "Invalidate and stop the active device discovery process."
  (let ((process elogcat--device-query-process))
    (setq elogcat--device-query-process nil)
    (when (process-live-p process)
      (delete-process process))))

(defun elogcat--cancel-clear ()
  "Invalidate and stop the active device-log clear process."
  (let ((process elogcat--clear-process))
    (setq elogcat--clear-process nil)
    (when (process-live-p process)
      (delete-process process))))

(defun elogcat--clear-finished (process _event)
  "Restart Logcat after asynchronous clear PROCESS succeeds."
  (when-let* ((buffer (process-get process 'elogcat-target-buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (eq process elogcat--clear-process)
        (setq elogcat--clear-process nil)
        (if (and (equal (process-get process 'elogcat-device-serial)
                        elogcat-device-serial)
                 (eq (process-status process) 'exit)
                 (= (process-exit-status process) 0))
            (progn (elogcat-stop) (elogcat))
          (setq elogcat-stream-state 'error
                elogcat-stream-error "Unable to clear device logs")
          (force-mode-line-update t))))))

(defun elogcat-erase-buffer ()
  "Clear device logs and the local backlog asynchronously."
  (interactive)
  (unless (process-live-p elogcat--clear-process)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (setq elogcat--records nil elogcat--records-tail nil
            elogcat--unresolved-records nil elogcat--backlog-size 0
            elogcat-pending-output "" elogcat-stream-state 'clearing))
    (setq elogcat--clear-process
          (make-process
           :name "elogcat-clear" :buffer nil :noquery t
           :command (apply #'elogcat--adb-command
                           (append (list "shell")
                                   (split-string-and-unquote
                                    elogcat-logcat-command)
                                   (list "-c")))
           :sentinel #'elogcat--clear-finished))
    (process-put elogcat--clear-process
                 'elogcat-target-buffer (current-buffer))
    (process-put elogcat--clear-process
                 'elogcat-device-serial elogcat-device-serial)
    (force-mode-line-update t)))

(defun elogcat--redraw-unless-paused ()
  "Redraw the retained backlog unless display updates are paused."
  (unless elogcat-paused
    (elogcat--render-backlog))
  (force-mode-line-update))

(defun elogcat-show-status ()
  "Show current Logcat state in the echo area."
  (interactive)
  (message
   "elogcat: %s; query=%S%s; package=%S (%d processes); level=%s; %d records"
   (if elogcat-paused "paused" (if elogcat-follow-tail "live" "hold"))
   elogcat-query-filter (if elogcat-query-match-case " [case]" "")
   elogcat-package-filter
   (if (hash-table-p elogcat--process-table)
       (hash-table-count elogcat--process-table)
     0)
   elogcat-min-level (length elogcat--records)))

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
     :raw line :message line :message-group (elogcat--new-message-group line)
     :system-p t))
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
                  (elogcat--extend-message-group
                   (elogcat-record-message-group previous)
                   (elogcat-record-message record))
                (elogcat--new-message-group (elogcat-record-message record))))
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
      :message-group
      (or (and previous
               (elogcat--extend-message-group
                (elogcat-record-message-group previous) line))
          (elogcat--new-message-group line))
      :message line)))))

(defun elogcat--record-matches-p (record)
  "Return non-nil when RECORD passes current filters."
  (if (elogcat-record-system-p record)
      t
    (let* ((level (elogcat-record-level record))
           (minimum (or (cl-position elogcat-min-level elogcat-level-priority
                                     :test #'string=) 0)))
      (and (elogcat--query-matches-p record)
           (or (null level)
               (>= (or (cl-position level elogcat-level-priority
                                    :test #'string=) 0)
                   minimum))))))

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
    (when trimmed
      (let ((retained (make-hash-table :test #'eq)))
        (dolist (record elogcat--records)
          (puthash record t retained))
        (setq elogcat--unresolved-records
              (seq-filter (lambda (record) (gethash record retained))
                          elogcat--unresolved-records))))
    (unless elogcat--records
      (setq elogcat--records-tail nil))
    trimmed))

(defun elogcat--format-fields (record)
  "Return RECORD formatted according to `elogcat-visible-fields'."
  (if (memq 'raw elogcat-visible-fields)
      (elogcat-record-raw record)
    (string-join
     (delq nil
           (mapcar
            (lambda (field)
              (pcase field
                ('timestamp (elogcat-record-timestamp record))
                ('pid-tid (when (elogcat-record-pid record)
                            (format "%s/%s" (elogcat-record-pid record)
                                    (or (elogcat-record-tid record) "?"))))
                ('application (car (elogcat-record-application-ids record)))
                ('process (elogcat-record-process-name record))
                ('level (elogcat-record-level record))
                ('tag (elogcat-record-tag record))
                ('message (elogcat-record-message record))
                (_ nil)))
            elogcat-visible-fields))
     " ")))

(defun elogcat--stack-frame-p (record)
  "Return non-nil when RECORD is a Java or Kotlin stack frame."
  (string-match-p "^[[:space:]]*at .+(.*:[0-9]+)"
                  (or (elogcat-record-message record) "")))

(defvar elogcat-stack-frame-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'elogcat-visit-source-mouse)
    map)
  "Mouse map installed on source stack frames.")

(defun elogcat--format-record (record)
  "Return RECORD formatted for insertion into the Logcat buffer."
  (let* ((level (elogcat-record-level record))
         (face (cdr (or (assoc level elogcat-face-alist)
                        (assoc "V" elogcat-face-alist))))
         (occurrence (and (or (and (member level '("E" "F" "A"))
                                   (string-match-p elogcat--threadtime-regexp
                                                   (elogcat-record-raw record)))
                              (elogcat--stack-frame-p record)
                              (string-match-p "^[[:space:]]*\\(?:Caused by:\\|Suppressed:\\)"
                                              (or (elogcat-record-message record) "")))
                          record)))
    (propertize (concat (elogcat--format-fields record) "\n")
                'face face 'elogcat-record record
                'elogcat-occurrence occurrence
                'mouse-face (and (elogcat--stack-frame-p record) 'highlight)
                'keymap (and (elogcat--stack-frame-p record)
                             elogcat-stack-frame-map)
                'help-echo (and (elogcat--stack-frame-p record)
                                "RET: visit source location")
                'rear-nonsticky t)))

(defun elogcat--record-hidden-by-fold-p (record)
  "Return non-nil when RECORD is hidden by a collapsed exception group."
  (and (gethash (elogcat-record-message-group record) elogcat--collapsed-groups)
       (elogcat--stack-frame-p record)))

(defun elogcat--fold-summary (record)
  "Return a visible folded-stack summary associated with RECORD."
  (propertize "    … stack frames folded (TAB to expand)\n"
              'face 'shadow 'elogcat-record record
              'help-echo "TAB: expand stack frames"
              'rear-nonsticky t))

(defun elogcat--insert-records (records)
  "Insert matching, non-folded RECORDS at the end of the current buffer."
  (let ((summarized (make-hash-table :test #'eq)) chunks)
    (dolist (record records)
      (when (elogcat--record-matches-p record)
        (let ((group (elogcat-record-message-group record)))
          (if (elogcat--record-hidden-by-fold-p record)
              (unless (gethash group summarized)
                (puthash group t summarized)
                (push (elogcat--fold-summary record) chunks))
            (push (elogcat--format-record record) chunks)))))
    (setq chunks (nreverse chunks))
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
  (when (derived-mode-p 'elogcat-mode)
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

(defun elogcat--query-group-became-visible-p (records)
  "Return non-nil when RECORDS made an older message group match the query."
  (when (and elogcat--query-predicate records)
    (let ((first (car records)))
      (and (elogcat-record-message-group first)
           (eq (elogcat-record-message-group first)
               (and elogcat--records-tail
                    (elogcat-record-message-group
                     (car elogcat--records-tail))))
           (elogcat--query-matches-p first)))))

(defun elogcat-process-filter (process output)
  "Retain and display records from PROCESS parsed from OUTPUT."
  (when-let* ((buffer (get-buffer elogcat-buffer)))
    (with-current-buffer buffer
      (when (eq process elogcat--stream-process)
        (setq elogcat-stream-state 'live elogcat--reconnect-attempt 0)
        (let* ((old-max (point-max))
             (following-windows
              (and elogcat-follow-tail
                   (cl-loop for window in (get-buffer-window-list buffer nil t)
                            when (>= (window-point window) old-max)
                            collect window)))
             (records (elogcat--consume-output output))
             (query-group-became-visible
              (elogcat--query-group-became-visible-p records))
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
          (if (or trimmed new-package-group query-group-became-visible
                  (and (> (hash-table-count elogcat--collapsed-groups) 0)
                       records))
              (elogcat--render-backlog)
            (elogcat--insert-records records))
          (dolist (window following-windows)
            (set-window-point window (point-max)))))))))


(defun elogcat--parse-devices (output)
  "Return connected adb devices parsed from OUTPUT."
  (let (devices)
    (dolist (line (cdr (split-string output "\n" t)))
      (when (string-match
             "^\\([^[:space:]]+\\)[[:space:]]+device\\(?:[[:space:]]+\\(.*\\)\\)?$"
             line)
        (let* ((serial (match-string 1 line))
               (properties (or (match-string 2 line) ""))
               (model (and (string-match "model:\\([^[:space:]]+\\)" properties)
                           (match-string 1 properties))))
          (push (cons serial (or model serial)) devices))))
    (nreverse devices)))

(defun elogcat--choose-device (devices)
  "Return a device serial selected from DEVICES."
  (cond
   ((null devices) nil)
   ((and elogcat-default-device-serial
         (assoc elogcat-default-device-serial devices))
    elogcat-default-device-serial)
   ((= (length devices) 1) (caar devices))
   (noninteractive (caar devices))
   (t (car (rassoc (completing-read "Android device: "
                                    (mapcar #'cdr devices) nil t)
                   devices)))))

(defun elogcat--devices-sentinel (process _event)
  "Select a device from adb discovery PROCESS and start Logcat."
  (let ((target (process-get process 'elogcat-target-buffer))
        (output-buffer (process-buffer process)))
    (unwind-protect
        (when (buffer-live-p target)
          (with-current-buffer target
            (when (eq process elogcat--device-query-process)
              (setq elogcat--device-query-process nil)
              (if (and (eq (process-status process) 'exit)
                     (= (process-exit-status process) 0))
                (let* ((devices (elogcat--parse-devices
                                 (with-current-buffer output-buffer
                                   (buffer-string))))
                       (serial (elogcat--choose-device devices)))
                  (setq elogcat--devices devices)
                  (if serial
                      (progn
                        (setq elogcat-device-serial serial
                              elogcat-device-name (cdr (assoc serial devices)))
                        (elogcat--start-stream))
                    (setq elogcat-stream-state 'offline
                          elogcat-stream-error "No connected Android device")
                    (force-mode-line-update t)))
              (setq elogcat-stream-state 'error
                    elogcat-stream-error "adb devices failed")
              (force-mode-line-update t)))))
      (when (buffer-live-p output-buffer) (kill-buffer output-buffer)))))

(defun elogcat--discover-device ()
  "Discover connected Android devices asynchronously."
  (elogcat--cancel-device-query)
  (setq elogcat-stream-state 'connecting elogcat-stream-error nil)
  (let ((output-buffer (generate-new-buffer " *elogcat-devices*")))
    (setq elogcat--device-query-process
          (make-process :name "elogcat-devices" :buffer output-buffer
                        :command '("adb" "devices" "-l")
                        :connection-type 'pipe :noquery t
                        :sentinel #'elogcat--devices-sentinel))
    (process-put elogcat--device-query-process
                 'elogcat-target-buffer (current-buffer))))

(defun elogcat-select-device (serial)
  "Switch the current Logcat session to adb device SERIAL."
  (interactive
   (list
    (if elogcat--devices
        (let ((name (completing-read "Android device: "
                                     (mapcar #'cdr elogcat--devices) nil t)))
          (car (rassoc name elogcat--devices)))
      (read-string "Device serial: " elogcat-device-serial))))
  (elogcat--cancel-device-query)
  (elogcat--cancel-clear)
  (when (timerp elogcat--reconnect-timer)
    (cancel-timer elogcat--reconnect-timer))
  (setq elogcat--reconnect-timer nil elogcat--intentional-stop t)
  (when (process-live-p elogcat--stream-process)
    (process-put elogcat--stream-process 'elogcat-intentional-stop t)
    (delete-process elogcat--stream-process))
  (elogcat--stop-process-monitor)
  (setq elogcat-device-serial serial elogcat-device-name serial
        elogcat--process-table (make-hash-table :test #'equal)
        elogcat--unresolved-records nil elogcat--intentional-stop nil
        elogcat--reconnect-attempt 0)
  (elogcat--start-stream))

(defun elogcat-choose-device ()
  "Rediscover connected devices and switch the current session."
  (interactive)
  (elogcat--cancel-device-query)
  (elogcat--cancel-clear)
  (when (timerp elogcat--reconnect-timer)
    (cancel-timer elogcat--reconnect-timer))
  (setq elogcat--reconnect-timer nil elogcat--intentional-stop t)
  (when (process-live-p elogcat--stream-process)
    (process-put elogcat--stream-process 'elogcat-intentional-stop t)
    (delete-process elogcat--stream-process))
  (elogcat--stop-process-monitor)
  (setq elogcat-device-serial nil elogcat-device-name nil
        elogcat--process-table (make-hash-table :test #'equal)
        elogcat--unresolved-records nil elogcat--intentional-stop nil)
  (elogcat--discover-device))

(defun elogcat--schedule-reconnect ()
  "Schedule a bounded reconnect after an unexpected stream exit."
  (when (and (not elogcat--intentional-stop)
             (< elogcat--reconnect-attempt elogcat-auto-reconnect-attempts))
    (cl-incf elogcat--reconnect-attempt)
    (setq elogcat-stream-state 'reconnecting
          elogcat--reconnect-timer
          (run-at-time (min 10 (expt 2 (1- elogcat--reconnect-attempt))) nil
                       (lambda (buffer)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (setq elogcat--reconnect-timer nil)
                             (unless (process-live-p elogcat--stream-process)
                               (elogcat--start-stream)))))
                       (current-buffer)))))

(defun elogcat-reconnect ()
  "Reconnect the current Logcat stream without clearing the backlog."
  (interactive)
  (elogcat--cancel-device-query)
  (elogcat--cancel-clear)
  (when (timerp elogcat--reconnect-timer)
    (cancel-timer elogcat--reconnect-timer))
  (setq elogcat--reconnect-timer nil elogcat--intentional-stop t)
  (when (process-live-p elogcat--stream-process)
    (process-put elogcat--stream-process 'elogcat-intentional-stop t)
    (delete-process elogcat--stream-process))
  (setq elogcat--intentional-stop nil elogcat--reconnect-attempt 0)
  (elogcat--stop-process-monitor)
  (elogcat--start-stream))

(defun elogcat-process-sentinel (process event)
  "Update Logcat status after PROCESS changes state, using EVENT for details."
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (unless (process-live-p process)
        (when (eq process elogcat--stream-process)
          (setq elogcat--stream-process nil)
          (let ((intentional (or elogcat--intentional-stop
                                 (process-get process
                                              'elogcat-intentional-stop))))
            (setq elogcat-stream-state (if intentional 'stopped 'offline)
                  elogcat-stream-error
                  (unless intentional (string-trim event)))
            (elogcat--stop-process-monitor)
            (unless intentional (elogcat--schedule-reconnect)))))
      (force-mode-line-update t))))

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

(defun elogcat-visit-source ()
  "Visit the source location referenced by the stack frame at point."
  (interactive)
  (let* ((record (elogcat--record-at-position (point)))
         (message (and record (elogcat-record-message record))))
    (unless (and message
                 (string-match "(\\([^():/]+\\.[[:alnum:]]+\\):\\([0-9]+\\))"
                               message))
      (user-error "No source location at point"))
    (let* ((name (match-string 1 message))
           (line (string-to-number (match-string 2 message)))
           (root (or elogcat-project-root default-directory))
           (files (condition-case nil
                      (project-files (project-current nil root))
                    (error (directory-files-recursively
                            root (concat (regexp-quote name) "\\'")))))
           (matches (seq-filter
                     (lambda (file) (string-suffix-p name file)) files))
           (file (cond ((= (length matches) 1) (car matches))
                       (matches (completing-read "Source file: " matches nil t))
                       (t nil))))
      (unless file (user-error "Source file not found: %s" name))
      (find-file (if (file-name-absolute-p file) file
                   (expand-file-name file root)))
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun elogcat-visit-source-mouse (event)
  "Visit the source location clicked in mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (elogcat-visit-source))

(defun elogcat-toggle-exception-fold ()
  "Toggle stack-frame folding for the message group at point."
  (interactive)
  (when-let* ((record (elogcat--record-at-position (point)))
              (group (elogcat-record-message-group record)))
    (if (gethash group elogcat--collapsed-groups)
        (remhash group elogcat--collapsed-groups)
      (puthash group t elogcat--collapsed-groups))
    (elogcat--render-backlog)))

(defun elogcat-toggle-all-exception-folds ()
  "Collapse all exception groups, or expand them when any are collapsed."
  (interactive)
  (if (> (hash-table-count elogcat--collapsed-groups) 0)
      (clrhash elogcat--collapsed-groups)
    (dolist (record elogcat--records)
      (when (elogcat--stack-frame-p record)
        (puthash (elogcat-record-message-group record) t
                 elogcat--collapsed-groups))))
  (elogcat--render-backlog))

(defun elogcat-next-occurrence ()
  "Move to the next error, assertion, or stack frame, wrapping at the end."
  (interactive)
  (elogcat--move-occurrence 1))

(defun elogcat-previous-occurrence ()
  "Move to the previous error, assertion, or stack frame, wrapping at the start."
  (interactive)
  (elogcat--move-occurrence -1))

(defun elogcat-select-visible-fields (preset)
  "Select a display-field PRESET and redraw the backlog."
  (interactive
   (list (completing-read "Log display: "
                          '("Raw" "Compact" "Process" "Full") nil t)))
  (setq elogcat-visible-fields
        (pcase preset
          ("Compact" '(level tag message))
          ("Process" '(timestamp application process level tag message))
          ("Full" '(timestamp pid-tid application process level tag message))
          (_ '(raw))))
  (elogcat--render-backlog))

(defun elogcat-use-saved-filter (name)
  "Apply the saved filter named NAME."
  (interactive
   (list (completing-read "Saved filter: "
                          (mapcar #'car elogcat-saved-filters) nil t)))
  (elogcat-set-query-filter (alist-get name elogcat-saved-filters nil nil
                                       #'string=)))

(defun elogcat-save-current-filter (name)
  "Save the current query filter under NAME."
  (interactive "sFilter name: ")
  (setf (alist-get name elogcat-saved-filters nil nil #'string=)
        (or elogcat-query-filter ""))
  (customize-save-variable 'elogcat-saved-filters elogcat-saved-filters)
  (message "elogcat: saved filter %s" name))

(defun elogcat-select-filter-history (query)
  "Apply QUERY selected from filter history."
  (interactive
   (list (completing-read "Recent filter: "
                          (delete-dups elogcat-query-filter-history)
                          nil t)))
  (elogcat-set-query-filter query))

(transient-define-prefix elogcat-dispatch ()
  "Show commands for the current Logcat session."
  [["Stream"
    ("SPC" "Pause/resume" elogcat-toggle-pause :transient t)
    ("f" "Follow tail" elogcat-toggle-follow-tail :transient t)
    ("r" "Reconnect" elogcat-reconnect)
    ("c" "Clear logs" elogcat-erase-buffer)]
   ["Filter"
    ("/" "Query" elogcat-set-query-filter)
    ("l" "Minimum level" elogcat-set-level)
    ("P" "Select Mine" elogcat-select-mine)
    ("M-c" "Match case" elogcat-toggle-query-match-case :transient t)
    ("h" "Recent query" elogcat-select-filter-history)
    ("N" "Named query" elogcat-use-saved-filter)
    ("S" "Save query" elogcat-save-current-filter)]
   ["View"
    ("V" "Display fields" elogcat-select-visible-fields)
    ("w" "Soft wrap" elogcat-toggle-soft-wrap :transient t)
    ("TAB" "Fold exception" elogcat-toggle-exception-fold :transient t)
    ("<backtab>" "Fold all" elogcat-toggle-all-exception-folds :transient t)
    ("o" "Occur" occur)]
   ["Navigate"
    ("n" "Next occurrence" elogcat-next-occurrence :transient t)
    ("p" "Previous occurrence" elogcat-previous-occurrence :transient t)
    ("RET" "Visit source" elogcat-visit-source)
    ("D" "Select device" elogcat-choose-device)
    ("g" "Show status" elogcat-show-status :transient t)
    ("s" "Save buffer" elogcat-save-buffer)
    ("q" "Quit Logcat" elogcat-exit)]])

(defvar elogcat-mode-map nil
  "Keymap for elogcat minor mode.")

(unless elogcat-mode-map
  (setq elogcat-mode-map (make-sparse-keymap)))

(--each '(("SPC" . elogcat-toggle-pause)
          ("/" . elogcat-set-query-filter)
          ("?" . elogcat-dispatch)
          ("RET" . elogcat-visit-source)
          ("TAB" . elogcat-toggle-exception-fold)
          ("<backtab>" . elogcat-toggle-all-exception-folds)
          ("c" . elogcat-erase-buffer)
          ("D" . elogcat-choose-device)
          ("f" . elogcat-toggle-follow-tail)
          ("g" . elogcat-show-status)
          ("h" . elogcat-select-filter-history)
          ("l" . elogcat-set-level)
          ("N" . elogcat-use-saved-filter)
          ("C-c C-s" . elogcat-save-current-filter)
          ("n" . elogcat-next-occurrence)
          ("o" . occur)
          ("p" . elogcat-previous-occurrence)
          ("q" . elogcat-exit)
          ("r" . elogcat-reconnect)
          ("s" . elogcat-save-buffer)
          ("V" . elogcat-select-visible-fields)
          ("w" . elogcat-toggle-soft-wrap)
          ("M-c" . elogcat-toggle-query-match-case)
          ("P" . elogcat-select-mine))
  (define-key elogcat-mode-map (read-kbd-macro (car it)) (cdr it)))

(define-key elogcat-mode-map [remap next-line] #'elogcat-next-occurrence)
(define-key elogcat-mode-map [remap previous-line] #'elogcat-previous-occurrence)

(define-derived-mode elogcat-mode special-mode "Logcat"
  "Major mode for browsing a structured Android Logcat stream."
  (setq-local truncate-lines (not elogcat-soft-wrap)
              header-line-format '(:eval (elogcat--make-header-line))
              mode-line-process elogcat-mode-line
              elogcat-paused nil
              elogcat-follow-tail t
              elogcat-query-filter elogcat-default-query
              elogcat--query-predicate
              (and elogcat-default-query
                   (elogcat--query-compile elogcat-default-query))
              elogcat--redraw-function #'elogcat--redraw-unless-paused
              elogcat--process-table (make-hash-table :test #'equal)
              elogcat--unresolved-records nil
              elogcat--package-message-cache (make-hash-table :test #'eq)
              elogcat--collapsed-groups (make-hash-table :test #'eq)
              elogcat-visible-fields (copy-sequence elogcat-default-visible-fields)
              elogcat-device-serial elogcat-default-device-serial
              elogcat-stream-state 'stopped)
  (add-hook 'kill-buffer-hook #'elogcat--stop-process-monitor nil t)
  (buffer-disable-undo))

(defun elogcat-exit ()
  "Exit elogcat."
  (interactive)
  (let* ((buf (current-buffer))
         (proc elogcat--stream-process))
    (when (process-live-p proc)
      (process-put proc 'elogcat-intentional-stop t)
      (delete-process proc))
    (kill-buffer buf)))

(defun elogcat--android-mode-project-package ()
  "Return android-mode's application ID for the current project context."
  (when (and (fboundp 'android-root)
             (fboundp 'android--flavor-appid))
    (when-let* ((root (ignore-errors (android-root))))
      (or
       (when (boundp 'android--selected-targets)
         (when-let* ((target (cdr (assoc root android--selected-targets))))
           (ignore-errors
             (android--flavor-appid (car target) (cdr target)))))
       (when (and buffer-file-name
                  (fboundp 'android--target-for-source-file))
         (when-let* ((entry (ignore-errors
                              (android--target-for-source-file
                               buffer-file-name root))))
           (plist-get entry :application-id)))
       (when (fboundp 'android--get-flavors)
         (let ((application-ids
                (delete-dups
                 (delq nil
                       (mapcar (lambda (entry)
                                 (and (listp entry)
                                      (plist-get entry :application-id)))
                               (ignore-errors
                                 (let ((default-directory root))
                                   (android--get-flavors))))))))
           (and (= (length application-ids) 1)
                (car application-ids))))))))

(defun elogcat--set-mine (package)
  "Set PACKAGE as the application represented by `package:mine'."
  (setq elogcat-package-filter (unless (string-empty-p package) package))
  (elogcat--rebuild-package-message-cache)
  (elogcat--refresh-process-table)
  (elogcat--redraw-unless-paused)
  (message "elogcat: package:mine is %s"
           (or elogcat-package-filter "not selected")))

(defun elogcat--select-mine-from-metadata (metadata)
  "Prompt for Mine using package METADATA."
  (when (derived-mode-p 'elogcat-mode)
    (let* ((observed (cl-loop for record in elogcat--records
                              append (elogcat-record-application-ids record)))
           (packages (delete-dups
                      (append observed (plist-get metadata :packages))))
           (package (completing-read "Project application ID: " packages
                                     nil nil nil nil elogcat-package-filter)))
      (elogcat--set-mine package))))

(defun elogcat-select-mine ()
  "Select the application ID represented by `package:mine' asynchronously."
  (interactive)
  (if-let* ((metadata (elogcat--cached-package-metadata)))
      (elogcat--select-mine-from-metadata metadata)
    (message "elogcat: loading installed applications…")
    (elogcat--ensure-package-metadata
     #'elogcat--select-mine-from-metadata)))

(defun elogcat-stop ()
  "Stop the adb Logcat process and package process monitor."
  (when-let* ((buffer (get-buffer elogcat-buffer)))
    (with-current-buffer buffer
      (setq elogcat--intentional-stop t)
      (elogcat--cancel-device-query)
      (elogcat--cancel-clear)
      (when (timerp elogcat--reconnect-timer)
        (cancel-timer elogcat--reconnect-timer))
      (setq elogcat--reconnect-timer nil)
      (elogcat--stop-process-monitor)
      (when (process-live-p elogcat--stream-process)
        (process-put elogcat--stream-process 'elogcat-intentional-stop t)
        (delete-process elogcat--stream-process)))))

(defun elogcat--start-stream ()
  "Start Logcat for the selected device and current session options."
  (when (and elogcat-device-serial
             (not (process-live-p elogcat--stream-process)))
    (setq elogcat--intentional-stop nil elogcat-stream-state 'connecting
          elogcat-stream-error nil)
    (let* ((tail (or elogcat--start-tail ""))
           (arguments (append (list "shell")
                              (split-string-and-unquote elogcat-logcat-command)
                              (unless (s-contains? "-b" elogcat-logcat-command)
                                (list "-s"))
                              tail))
           (process (make-process
                     :name "elogcat" :buffer (current-buffer)
                     :command (apply #'elogcat--adb-command arguments)
                     :connection-type 'pipe :noquery t
                     :filter #'elogcat-process-filter
                     :sentinel #'elogcat-process-sentinel)))
      (setq elogcat--stream-process process
            elogcat-stream-state 'connecting)
      (set-process-query-on-exit-flag process nil)
      (elogcat--start-process-monitor)
      (force-mode-line-update t))))

;;;###autoload
(defun elogcat (&optional arg)
  "Start or display the structured adb Logcat buffer.
Numeric ARG requests that many historical lines; a bare universal argument
requests complete available history."
  (interactive "P")
  (let* ((source-buffer (current-buffer))
         (existing (get-buffer elogcat-buffer))
         (new-session (not (buffer-live-p existing)))
         (buffer (get-buffer-create elogcat-buffer))
         (project-package
          (with-current-buffer source-buffer
            (and elogcat-project-package-function
                 (funcall elogcat-project-package-function))))
         (project-root
          (with-current-buffer source-buffer
            (or (when-let* ((project (project-current nil)))
                  (expand-file-name (project-root project)))
                default-directory))))
    (with-current-buffer buffer
      (when new-session (elogcat-mode))
      (setq elogcat-project-root (or elogcat-project-root project-root)
            elogcat-package-filter (or elogcat-package-filter project-package)
            elogcat--start-tail
            (cond ((consp arg) nil)
                  (arg (list "-T" (number-to-string
                                   (prefix-numeric-value arg))))
                  (elogcat-default-tail
                   (list "-T" (number-to-string elogcat-default-tail)))
                  (t nil)))
      (elogcat--rebuild-package-message-cache)
      (unless (process-live-p elogcat--stream-process)
        (if elogcat-device-serial
            (elogcat--start-stream)
          (elogcat--discover-device))))
    (switch-to-buffer buffer)
    (goto-char (point-max))))

(defun elogcat-save-buffer ()
  "Save the current elogcat buffer to a file and stop the logcat process."
  (interactive)
  (save-buffer)
  (elogcat-stop))

(provide 'elogcat)
;;; elogcat.el ends here
