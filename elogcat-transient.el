;;; elogcat-transient.el --- Transient menu for elogcat  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Youngjoo Lee

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Optional command menu for `elogcat-mode'.

;;; Code:
(require 'elogcat)

(defun elogcat-transient--define ()
  "Define the Logcat menu after loading the optional Transient package."
  (unless (fboundp 'elogcat-transient-menu)
    (unless (require 'transient nil t)
      (user-error "Install the transient package to use the Logcat menu"))
    (eval
     '(transient-define-prefix elogcat-transient-menu ()
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
        ("q" "Quit Logcat" elogcat-exit)]]))))

;;;###autoload
(defun elogcat-transient ()
  "Show commands for the current Logcat session."
  (interactive)
  (elogcat-transient--define)
  (funcall (intern "elogcat-transient-menu")))

(provide 'elogcat-transient)
;;; elogcat-transient.el ends here
