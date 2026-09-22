;;; elogcat-tests.el --- Tests for elogcat  -*- lexical-binding: t; -*-

(require 'ert)
(require 'elogcat)

(defconst elogcat-tests--debug
  "09-18 12:34:56.789  1234  5678 D DemoTag: debug message")

(defconst elogcat-tests--error
  "09-18 12:34:57.000  1234  5678 E DemoTag: fatal problem")

(defconst elogcat-tests--frame
  "09-18 12:34:57.001  1234  5678 E DemoTag:     at demo.Main.run(Main.kt:42)")

(defconst elogcat-tests--process-query
  (concat "__ELOGCAT_PACKAGES__\n"
          "package:com.example.app uid:10123\n"
          "package:com.shared.one uid:10124\n"
          "package:com.shared.two uid:10124\n"
          "__ELOGCAT_PROCESSES__\n"
          "UID PID NAME\n"
          "10123 1234 com.example.app:remote\n"
          "10124 2345 shared.process\n"))

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

(ert-deftest elogcat-process-query-maps-packages-to-running-processes ()
  "Package UIDs map main, remote, and shared-UID processes to application IDs."
  (let* ((table (elogcat--parse-process-query elogcat-tests--process-query))
         (remote (gethash "1234" table))
         (shared (gethash "2345" table)))
    (should (equal (elogcat-process-info-process-name remote)
                   "com.example.app:remote"))
    (should (equal (elogcat-process-info-application-ids remote)
                   '("com.example.app")))
    (should (equal (sort (copy-sequence
                          (elogcat-process-info-application-ids shared))
                         #'string<)
                   '("com.shared.one" "com.shared.two")))))

(ert-deftest elogcat-package-filter-keeps-system-and-assert-crash-messages ()
  "System markers and Assert proxy crashes survive package filtering."
  (elogcat-tests--with-buffer
   (setq elogcat-package-filter "com.example.app")
   (let* ((system (elogcat--parse-record "--------- beginning of main"))
          (crash (elogcat--parse-record
                  "09-18 12:34:59.000  9999  9999 A DEBUG: pid: 1234, name: app  >>> com.example.app <<<")))
     (elogcat--rebuild-package-message-cache (list system crash))
     (setq elogcat-min-level "A"
           elogcat-include-filter-regexp "not-present"
           elogcat-exclude-filter-regexp ".*")
     (should (elogcat-record-system-p system))
     (should (elogcat--record-matches-p system))
     (setq elogcat-include-filter-regexp nil
           elogcat-exclude-filter-regexp nil)
     (should (elogcat--record-matches-p crash)))))

(ert-deftest elogcat-package-filter-uses-structured-application-id ()
  "Package filtering matches application IDs exactly rather than PID or text."
  (elogcat-tests--with-buffer
   (setq elogcat--process-table
         (elogcat--parse-process-query elogcat-tests--process-query)
         elogcat-package-filter "com.example.app")
   (let ((record (elogcat--parse-record elogcat-tests--debug)))
     (should (equal (elogcat-record-application-ids record)
                    '("com.example.app")))
     (should (elogcat--record-matches-p record))
     (setq elogcat-package-filter "com.example")
     (should-not (elogcat--record-matches-p record)))))

(ert-deftest elogcat-process-query-supports-android-user-names ()
  "Legacy ps user names are normalized to package-manager numeric UIDs."
  (let* ((output (concat "__ELOGCAT_PACKAGES__\n"
                         "package:com.example.app uid:10123\n"
                         "__ELOGCAT_PROCESSES__\n"
                         "USER PID NAME\n"
                         "u0_a123 3456 com.example.app\n"))
         (info (gethash "3456" (elogcat--parse-process-query output))))
    (should (equal (elogcat-process-info-application-ids info)
                   '("com.example.app")))))

(ert-deftest elogcat-process-refresh-enriches-continuations ()
  "A delayed process mapping enriches headers and their continuation lines."
  (elogcat-tests--with-buffer
   (let* ((header (elogcat--parse-record elogcat-tests--error))
          (frame (elogcat--parse-record "    at demo.Main.run(Main.kt:42)"
                                        header)))
     (setq elogcat--records (list header frame))
     (elogcat--update-process-table
      (elogcat--parse-process-query elogcat-tests--process-query))
     (should (equal (elogcat-record-application-ids header)
                    '("com.example.app")))
     (should (equal (elogcat-record-application-ids frame)
                    '("com.example.app"))))))

(ert-deftest elogcat-process-refresh-enriches-retained-records ()
  "A new PID mapping updates retained records after an application restart."
  (elogcat-tests--with-buffer
   (let ((record (elogcat--parse-record elogcat-tests--debug)))
     (setq elogcat--records (list record))
     (should-not (elogcat-record-application-ids record))
     (elogcat--update-process-table
      (elogcat--parse-process-query elogcat-tests--process-query))
     (should (equal (elogcat-record-application-ids record)
                    '("com.example.app"))))))

(ert-deftest elogcat-package-filter-keeps-proxy-crash-message ()
  "Error groups mentioning a package survive proxy-process filtering."
  (elogcat-tests--with-buffer
   (setq elogcat-package-filter "com.example.app")
   (let* ((header (elogcat--parse-record
                   "09-18 12:34:58.000  9999  9999 E AndroidRuntime: FATAL EXCEPTION: main"))
          (process (elogcat--parse-record
                    "09-18 12:34:58.000  9999  9999 E AndroidRuntime: Process: com.example.app, PID: 1234"
                    header)))
     (elogcat--rebuild-package-message-cache (list header process))
     (should (eq (elogcat-record-message-group header)
                 (elogcat-record-message-group process)))
     (should (elogcat--record-matches-p header))
     (should (elogcat--record-matches-p process)))))

(ert-deftest elogcat-package-toggle-does-not-restart-or-clear ()
  "Changing package filters preserves the Logcat process and backlog."
  (elogcat-tests--with-buffer
   (let ((record (elogcat--parse-record elogcat-tests--debug))
         refreshed)
     (setq elogcat--records (list record))
     (cl-letf (((symbol-function 'elogcat--refresh-process-table)
                (lambda () (setq refreshed t)))
               ((symbol-function 'elogcat--start-process-monitor)
                (lambda () (ert-fail "package toggle restarted monitor")))
               ((symbol-function 'elogcat--stop-process-monitor)
                (lambda () (ert-fail "package toggle stopped monitor")))
               ((symbol-function 'message) #'ignore))
       (elogcat-toggle-package "com.example.app")
       (should refreshed)
       (should (equal elogcat--records (list record)))
       (setq refreshed nil)
       (elogcat-toggle-package "com.example.app")
       (should refreshed)
       (should (equal elogcat--records (list record)))))))

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
