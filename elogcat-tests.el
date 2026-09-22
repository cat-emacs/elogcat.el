;;; elogcat-tests.el --- Tests for elogcat  -*- lexical-binding: t; -*-

(require 'ert)
(require 'elogcat)

(defconst elogcat-tests--debug
  "09-18 12:34:56.789  1234  5678 D DemoTag: debug message")

(defconst elogcat-tests--error
  "09-18 12:34:57.000  1234  5678 E DemoTag: fatal problem")

(defconst elogcat-tests--frame
  "09-18 12:34:57.001  1234  5678 E DemoTag:     at demo.Main.run(Main.kt:42)")

(defmacro elogcat-tests--with-buffer (&rest body)
  "Evaluate BODY in an isolated Logcat buffer."
  `(let ((elogcat-buffer (generate-new-buffer-name " *elogcat-test*")))
     (unwind-protect
         (with-current-buffer (get-buffer-create elogcat-buffer)
           (elogcat-mode 1)
           (setq buffer-read-only t)
           ,@body)
       (when-let* ((buffer (get-buffer elogcat-buffer)))
         (kill-buffer buffer)))))

(ert-deftest elogcat-parse-threadtime-record ()
  "Threadtime fields are retained as a structured record."
  (let ((record (elogcat--parse-record elogcat-tests--debug)))
    (should (equal (elogcat-record-timestamp record) "09-18 12:34:56.789"))
    (should (equal (elogcat-record-pid record) "1234"))
    (should (equal (elogcat-record-tid record) "5678"))
    (should (equal (elogcat-record-level record) "D"))
    (should (equal (elogcat-record-tag record) "DemoTag"))
    (should (equal (elogcat-record-message record) "debug message"))))

(ert-deftest elogcat-consume-output-retains-incomplete-record ()
  "Process chunks only emit records after a complete line arrives."
  (elogcat-tests--with-buffer
   (should-not (elogcat--consume-output (substring elogcat-tests--debug 0 25)))
   (should-not (string-empty-p elogcat-pending-output))
   (let ((records (elogcat--consume-output
                   (concat (substring elogcat-tests--debug 25) "\n"))))
     (should (= (length records) 1))
     (should (equal (elogcat-record-raw (car records)) elogcat-tests--debug))
     (should (string-empty-p elogcat-pending-output)))))

(ert-deftest elogcat-continuation-inherits-filter-metadata ()
  "Stack trace continuations inherit level and tag from their header."
  (elogcat-tests--with-buffer
   (let* ((header (elogcat--parse-record elogcat-tests--error))
          (frame (elogcat--parse-record "    at demo.Main.run(Main.kt:42)"
                                        header)))
     (should (equal (elogcat-record-level frame) "E"))
     (should (equal (elogcat-record-tag frame) "DemoTag"))
     (setq elogcat-min-level "E"
           elogcat-include-filter-regexp "DemoTag")
     (should (elogcat--record-matches-p frame))
     (setq elogcat-min-level "F")
     (should-not (elogcat--record-matches-p frame)))))

(ert-deftest elogcat-filter-redraws-retained-records ()
  "Changing filters redraws existing records without restarting adb."
  (elogcat-tests--with-buffer
   (setq elogcat--records
         (mapcar #'elogcat--parse-record
                 (list elogcat-tests--debug elogcat-tests--error)))
   (elogcat--render-backlog)
   (should (string-match-p "debug message" (buffer-string)))
   (let ((buffer-read-only nil))
     (elogcat-set-level "E - Error"))
   (should-not (string-match-p "debug message" (buffer-string)))
   (should (string-match-p "fatal problem" (buffer-string)))
   (let ((buffer-read-only nil))
     (elogcat-set-filter "missing" 'elogcat-include-filter-regexp))
   (should (string-empty-p (buffer-string)))))

(ert-deftest elogcat-hold-redraw-preserves-current-record ()
  "Filtering while follow-tail is disabled preserves the selected record."
  (elogcat-tests--with-buffer
   (setq elogcat-follow-tail nil
         elogcat--records
         (mapcar #'elogcat--parse-record
                 (list elogcat-tests--debug elogcat-tests--error)))
   (elogcat--render-backlog)
   (goto-char (point-min))
   (forward-line 1)
   (let ((record (get-text-property (point) 'elogcat-record)))
     (elogcat--render-backlog)
     (should (eq (get-text-property (point) 'elogcat-record) record)))))

(ert-deftest elogcat-backlog-is-size-bounded ()
  "Old records are discarded when the configured backlog size is exceeded."
  (elogcat-tests--with-buffer
   (let* ((first (elogcat--parse-record elogcat-tests--debug))
          (second (elogcat--parse-record elogcat-tests--error))
          (elogcat-backlog-size (elogcat--record-size second)))
     (should (elogcat--add-records (list first second)))
     (should (equal elogcat--records (list second)))
     (should (<= elogcat--backlog-size elogcat-backlog-size)))))

(ert-deftest elogcat-pause-retains-and-resume-renders ()
  "Paused streams retain records and render them together on resume."
  (elogcat-tests--with-buffer
   (setq elogcat-paused t)
   (elogcat-process-filter nil (concat elogcat-tests--error "\n"))
   (should (= (length elogcat--records) 1))
   (should (string-empty-p (buffer-string)))
   (cl-letf (((symbol-function 'message) #'ignore))
     (elogcat-toggle-pause))
   (should-not elogcat-paused)
   (should (string-match-p "fatal problem" (buffer-string)))))

(ert-deftest elogcat-paused-filter-change-defers-redraw ()
  "Filter changes update state but leave a paused display frozen."
  (elogcat-tests--with-buffer
   (setq elogcat--records
         (mapcar #'elogcat--parse-record
                 (list elogcat-tests--debug elogcat-tests--error)))
   (elogcat--render-backlog)
   (setq elogcat-paused t)
   (let ((before (buffer-string)))
     (cl-letf (((symbol-function 'message) #'ignore))
       (elogcat-set-filter "missing" 'elogcat-include-filter-regexp))
     (should (equal (buffer-string) before)))
   (cl-letf (((symbol-function 'message) #'ignore))
     (elogcat-toggle-pause))
   (should (string-empty-p (buffer-string)))))

(ert-deftest elogcat-occurrence-navigation-wraps ()
  "Error records and stack frames form one wrapping navigation sequence."
  (elogcat-tests--with-buffer
   (let ((buffer-read-only nil))
     (elogcat--insert-records
      (mapcar #'elogcat--parse-record
              (list elogcat-tests--debug elogcat-tests--error
                    elogcat-tests--frame))))
   (goto-char (point-min))
   (elogcat-next-occurrence)
   (should (string-match-p "fatal problem" (thing-at-point 'line t)))
   (elogcat-next-occurrence)
   (should (string-match-p "Main.kt:42" (thing-at-point 'line t)))
   (elogcat-next-occurrence)
   (should (string-match-p "fatal problem" (thing-at-point 'line t)))
   (elogcat-previous-occurrence)
   (should (string-match-p "Main.kt:42" (thing-at-point 'line t)))))

(ert-deftest elogcat-mode-line-distinguishes-live-hold-and-paused ()
  "The mode line reports whether the visible stream follows its tail."
  (elogcat-tests--with-buffer
   (let ((buffer-read-only nil))
     (insert "one\ntwo\n"))
   (goto-char (point-max))
   (should (string-match-p "LIVE" (elogcat-make-status)))
   (goto-char (point-min))
   (should (string-match-p "HOLD" (elogcat-make-status)))
   (setq elogcat-paused t)
   (should (string-match-p "PAUSED" (elogcat-make-status)))))

(ert-deftest elogcat-tail-and-wrap-toggles-update-state ()
  "Tail following and soft wrapping are explicit toggles."
  (elogcat-tests--with-buffer
   (cl-letf (((symbol-function 'message) #'ignore))
     (should elogcat-follow-tail)
     (elogcat-toggle-follow-tail)
     (should-not elogcat-follow-tail)
     (should truncate-lines)
     (elogcat-toggle-soft-wrap)
     (should-not truncate-lines))))

(ert-deftest elogcat-mode-exposes-studio-style-controls ()
  "The mode map exposes pause, follow, wrap, and occurrence navigation."
  (should (eq (lookup-key elogcat-mode-map (kbd "SPC"))
              #'elogcat-toggle-pause))
  (should (eq (lookup-key elogcat-mode-map (kbd "f"))
              #'elogcat-toggle-follow-tail))
  (should (eq (lookup-key elogcat-mode-map (kbd "W"))
              #'elogcat-toggle-soft-wrap))
  (should (eq (lookup-key elogcat-mode-map (kbd "n"))
              #'elogcat-next-occurrence))
  (should (eq (lookup-key elogcat-mode-map (kbd "p"))
              #'elogcat-previous-occurrence)))

;;; elogcat-tests.el ends here
