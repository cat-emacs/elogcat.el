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

(defun elogcat--make-header-line ()
  "Return a concise Android Studio-style Logcat header."
  (concat
   " " (propertize "Mine:" 'face 'bold)
   " " (or elogcat-package-filter "not selected")
   "    " (propertize "Filter:" 'face 'bold)
   " " (or elogcat-query-filter "all messages")
   (when elogcat-show-key-hints
     (propertize "    / filter   l level   P mine   SPC pause   ? keys"
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
            (if (and elogcat-follow-tail (elogcat--at-tail-p)) "LIVE" "HOLD"))
          (if (equal elogcat-min-level "V")
              "ALL"
            (concat elogcat-min-level "+"))
          (if truncate-lines "" " · WRAP")
          (if elogcat-query-match-case " · CASE" "")))

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
   "elogcat: %s; query=%S%s; package=%S (%d processes); include=%S; exclude=%S; level=%s; %d records"
   (if elogcat-paused "paused" (if elogcat-follow-tail "live" "hold"))
   elogcat-query-filter (if elogcat-query-match-case " [case]" "")
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
    (let* ((text (elogcat--record-filter-text record))
           (level (elogcat-record-level record))
           (minimum (or (cl-position elogcat-min-level elogcat-level-priority
                                     :test #'string=) 0)))
      (and (elogcat--query-matches-p record)
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
          (if (or trimmed new-package-group query-group-became-visible)
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

(dolist (key '("C" "W" "i" "x" "I" "X" "L" "S" "F"
               "m" "s" "r" "e" "c" "k"))
  (define-key elogcat-mode-map (kbd key) nil))

(--each '(("SPC" . elogcat-toggle-pause)
          ("/" . elogcat-set-query-filter)
          ("?" . describe-mode)
          ("c" . elogcat-erase-buffer)
          ("f" . elogcat-toggle-follow-tail)
          ("g" . elogcat-show-status)
          ("l" . elogcat-set-level)
          ("n" . elogcat-next-occurrence)
          ("o" . occur)
          ("p" . elogcat-previous-occurrence)
          ("q" . elogcat-exit)
          ("s" . elogcat-save-buffer)
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
              elogcat--package-message-cache (make-hash-table :test #'eq))
  (add-hook 'kill-buffer-hook #'elogcat--stop-process-monitor nil t)
  (buffer-disable-undo))

(defun elogcat-exit ()
  "Exit elogcat."
  (interactive)
  (let* ((buf (current-buffer))
         (proc (get-buffer-process buf)))
    (when (process-live-p proc)
      (kill-process proc)
      (sleep-for 0.1))
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

(defun elogcat-select-mine (package)
  "Set PACKAGE as the application ID represented by `package:mine'.
This updates retained records without restarting the Logcat stream."
  (interactive
   (list (completing-read
          "Project application ID: "
          (delete-dups
           (append
            (cl-loop for record in elogcat--records
                     append (elogcat-record-application-ids record))
            (mapcar (lambda (name) (s-chop-prefix "package:" name))
                    (split-string
                     (string-trim
                      (shell-command-to-string
                       "adb shell pm list packages -3"))
                     "\n" t))))
          nil nil nil nil elogcat-package-filter)))
  (setq elogcat-package-filter (unless (string-empty-p package) package))
  (elogcat--rebuild-package-message-cache)
  (elogcat--refresh-process-table)
  (elogcat--redraw-unless-paused)
  (message "elogcat: package:mine is %s"
           (or elogcat-package-filter "not selected")))

(defalias 'elogcat-toggle-package #'elogcat-select-mine)

(defun elogcat-stop ()
  "Stop the adb Logcat process and package process monitor."
  (when-let* ((buffer (get-buffer elogcat-buffer)))
    (with-current-buffer buffer
      (when (derived-mode-p 'elogcat-mode)
        (elogcat--stop-process-monitor))))
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
    (let* ((session-state
            (when (derived-mode-p 'elogcat-mode)
              (list :package elogcat-package-filter
                    :query elogcat-query-filter
                    :level elogcat-min-level
                    :match-case elogcat-query-match-case
                    :include elogcat-include-filter-regexp
                    :exclude elogcat-exclude-filter-regexp
                    :follow elogcat-follow-tail
                    :truncate truncate-lines)))
           (project-package
            (or (plist-get session-state :package)
                (and elogcat-project-package-function
                     (funcall elogcat-project-package-function))))
           (tail-arg (cond
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
        (elogcat-mode)
        (when session-state
          (setq elogcat-query-filter (plist-get session-state :query)
                elogcat-min-level (plist-get session-state :level)
                elogcat-query-match-case (plist-get session-state :match-case)
                elogcat-include-filter-regexp (plist-get session-state :include)
                elogcat-exclude-filter-regexp (plist-get session-state :exclude)
                elogcat-follow-tail (plist-get session-state :follow)
                truncate-lines (plist-get session-state :truncate)
                elogcat--query-predicate
                (and elogcat-query-filter
                     (elogcat--query-compile elogcat-query-filter))))
        (setq elogcat-package-filter project-package)
        (elogcat--rebuild-package-message-cache)
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
