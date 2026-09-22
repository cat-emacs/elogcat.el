;;; elogcat-tests.el --- Tests for elogcat  -*- lexical-binding: t; -*-

(require 'ert)
(require 'elogcat)

(defconst elogcat-tests--debug
  "09-18 12:34:56.789  1234  5678 D DemoTag: debug message")

(defconst elogcat-tests--error
  "09-18 12:34:57.000  1234  5678 E DemoTag: fatal problem")

(defconst elogcat-tests--frame
  "09-18 12:34:57.001  1234  5678 E DemoTag:     at demo.Main.run(Main.kt:42)")

(defconst elogcat-tests--packages
  (concat "package:com.example.app uid:10123\n"
          "package:com.shared.one uid:10124\n"
          "package:com.shared.two uid:10124\n"))

(defconst elogcat-tests--processes
  (concat "UID PID NAME\n"
          "10123 1234 com.example.app:remote\n"
          "10124 2345 shared.process\n"
          "10124 2346 com.shared.one:worker\n"))

(defun elogcat-tests--process-table ()
  "Return process metadata parsed through the split query pipeline."
  (let ((metadata (elogcat--parse-package-output elogcat-tests--packages)))
    (elogcat--parse-process-output elogcat-tests--processes
                                   (plist-get metadata :uids))))

(defun elogcat-tests--query-record (&rest properties)
  "Return a structured record with PROPERTIES for query tests."
  (let ((record
         (make-elogcat-record
          :raw "09-18 12:34:56.789  1234  5678 W DemoTag: Hello World"
          :timestamp "09-18 12:34:56.789" :pid "1234" :tid "5678"
          :level "W" :tag "DemoTag" :message "Hello World"
          :application-ids '("com.example.app")
          :process-name "com.example.app:worker"
          :message-group (elogcat--new-message-group "Hello World"))))
    (while properties
      (let ((property (pop properties))
            (value (pop properties)))
        (pcase property
          (:raw (setf (elogcat-record-raw record) value))
          (:timestamp (setf (elogcat-record-timestamp record) value))
          (:level (setf (elogcat-record-level record) value))
          (:tag (setf (elogcat-record-tag record) value))
          (:message (setf (elogcat-record-message record) value))
          (:message-group (setf (elogcat-record-message-group record) value))
          (_ (error "Unsupported test record property: %S" property)))))
    record))

(defun elogcat-tests--query-matches (query record)
  "Return whether QUERY matches RECORD."
  (let ((elogcat--query-predicate (elogcat--query-compile query)))
    (elogcat--query-matches-p record)))

(defvar elogcat-tests--default-query nil
  "Default query installed by `elogcat-tests--with-buffer'.")

(defmacro elogcat-tests--with-buffer (&rest body)
  "Evaluate BODY in an isolated Logcat buffer."
  `(let ((elogcat-buffer (generate-new-buffer-name " *elogcat-test*"))
         (elogcat-default-query elogcat-tests--default-query))
     (unwind-protect
         (with-current-buffer (get-buffer-create elogcat-buffer)
           (elogcat-mode)
           ,@body)
       (when-let* ((buffer (get-buffer elogcat-buffer)))
         (kill-buffer buffer)))))

(ert-deftest elogcat-process-query-maps-packages-to-running-processes ()
  "Package UIDs map main, remote, and shared-UID processes to application IDs."
  (let* ((table (elogcat-tests--process-table))
         (remote (gethash "1234" table))
         (ambiguous (gethash "2345" table))
         (shared (gethash "2346" table)))
    (should (equal (elogcat-process-info-process-name remote)
                   "com.example.app:remote"))
    (should (equal (elogcat-process-info-application-ids remote)
                   '("com.example.app")))
    (should-not (elogcat-process-info-application-ids ambiguous))
    (should (equal (elogcat-process-info-application-ids shared)
                   '("com.shared.one")))))

(ert-deftest elogcat-mine-selection-is-not-an-independent-filter ()
  "Selecting Mine alone does not hide records when the query is clear."
  (elogcat-tests--with-buffer
   (setq elogcat-package-filter "com.example.other"
         elogcat--query-predicate nil)
   (should (elogcat--record-matches-p
            (elogcat-tests--query-record)))))

(ert-deftest elogcat-unresolved-mine-is-ignored-in-query-logic ()
  "Unresolved Mine shows all alone and leaves other query terms effective."
  (elogcat-tests--with-buffer
   (setq elogcat-package-filter nil)
   (let ((record (elogcat-tests--query-record)))
     (should (elogcat-tests--query-matches "package:mine" record))
     (should (elogcat-tests--query-matches "-package:mine" record))
     (should (elogcat-tests--query-matches
              "package:mine & tag:DemoTag" record))
     (should-not (elogcat-tests--query-matches
                  "package:mine & tag:Other" record))
     (should-not (elogcat-tests--query-matches
                  "package:mine | tag:Other" record)))))

(ert-deftest elogcat-project-package-uses-public-android-api ()
  "The default project resolver consumes only android-mode's public API."
  (let (called)
    (cl-letf (((symbol-function 'android-current-application-id)
               (lambda (&optional _prompt _file _root)
                 (setq called t)
                 "com.example.app")))
      (should (equal (elogcat--android-mode-project-package)
                     "com.example.app"))
      (should called))))

(ert-deftest elogcat-package-mine-keeps-system-and-assert-crash-messages ()
  "System markers and Assert proxy crashes survive package:mine queries."
  (elogcat-tests--with-buffer
   (setq elogcat-package-filter "com.example.app"
         elogcat--query-predicate (elogcat--query-compile "package:mine"))
   (let* ((system (elogcat--parse-record "--------- beginning of main"))
          (crash (elogcat--parse-record
                  "09-18 12:34:59.000  9999  9999 A DEBUG: pid: 1234, name: app  >>> com.example.app <<<")))
     (elogcat--rebuild-package-message-cache (list system crash))
     (setq elogcat-min-level "A")
     (should (elogcat-record-system-p system))
     (should (elogcat--record-matches-p system))
     (should (elogcat--record-matches-p crash)))))

(ert-deftest elogcat-package-mine-uses-structured-application-id ()
  "Package mine matches application IDs exactly rather than PID or text."
  (elogcat-tests--with-buffer
   (setq elogcat--process-table
         (elogcat-tests--process-table)
         elogcat-package-filter "com.example.app"
         elogcat--query-predicate (elogcat--query-compile "package:mine"))
   (let ((record (elogcat--parse-record elogcat-tests--debug)))
     (should (equal (elogcat-record-application-ids record)
                    '("com.example.app")))
     (should (elogcat--record-matches-p record))
     (setq elogcat-package-filter "com.example")
     (should-not (elogcat--record-matches-p record)))))

(ert-deftest elogcat-process-query-supports-android-user-names ()
  "Android ps user names are normalized to package-manager numeric UIDs."
  (let* ((metadata (elogcat--parse-package-output
                    "package:com.example.app uid:10123\n"))
         (table (elogcat--parse-process-output
                 "USER PID NAME\nu0_a123 3456 com.example.app\n"
                 (plist-get metadata :uids)))
         (info (gethash "3456" table)))
    (should (equal (elogcat-process-info-application-ids info)
                   '("com.example.app")))))

(ert-deftest elogcat-process-refresh-redraws-structured-query ()
  "Metadata arrival redraws package/process queries without a P filter."
  (elogcat-tests--with-buffer
   (let ((record (elogcat--parse-record elogcat-tests--debug))
         redrawn)
     (setq elogcat--records (list record)
           elogcat--query-predicate (elogcat--query-compile "package:example")
           elogcat--redraw-function (lambda () (setq redrawn t)))
     (elogcat--update-process-table
      (elogcat-tests--process-table))
     (should redrawn))))

(ert-deftest elogcat-process-refresh-enriches-continuations ()
  "A delayed process mapping enriches headers and their continuation lines."
  (elogcat-tests--with-buffer
   (let* ((header (elogcat--parse-record elogcat-tests--error))
          (frame (elogcat--parse-record "    at demo.Main.run(Main.kt:42)"
                                        header)))
     (setq elogcat--records (list header frame))
     (elogcat--update-process-table
      (elogcat-tests--process-table))
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
      (elogcat-tests--process-table))
     (should (equal (elogcat-record-application-ids record)
                    '("com.example.app"))))))

(ert-deftest elogcat-package-mine-keeps-proxy-crash-message ()
  "Package mine retains proxy-process errors mentioning the application ID."
  (elogcat-tests--with-buffer
   (setq elogcat-package-filter "com.example.app"
         elogcat--query-predicate (elogcat--query-compile "package:mine"))
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

(ert-deftest elogcat-select-mine-does-not-restart-or-clear ()
  "Changing package:mine preserves the Logcat process and backlog."
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
       (elogcat--set-mine "com.example.app")
       (should refreshed)
       (should (equal elogcat--records (list record)))
       (setq refreshed nil)
       (elogcat--set-mine "com.example.app")
       (should refreshed)
       (should (equal elogcat--records (list record)))))))

(ert-deftest elogcat-query-late-group-match-redraws-hidden-header ()
  "A later matching stack line reveals earlier lines in the same message group."
  (elogcat-tests--with-buffer
   (setq elogcat-query-filter "message:Main.kt"
         elogcat--query-predicate
         (elogcat--query-compile "message:Main.kt"))
   (elogcat-process-filter
    nil "09-18 12:34:58.000  1234  1234 E Demo: Failure\n")
   (should (string-empty-p (buffer-string)))
   (elogcat-process-filter
    nil "09-18 12:34:58.000  1234  1234 E Demo:     at demo.Main.run(Main.kt:42)\n")
   (should (string-match-p "Failure" (buffer-string)))
   (should (string-match-p "Main.kt:42" (buffer-string)))))

(ert-deftest elogcat-query-matches-studio-field-operators ()
  "Structured fields support contains, exact, regex, and negation operators."
  (elogcat-tests--with-buffer
   (let ((record (elogcat-tests--query-record)))
     (should (elogcat-tests--query-matches "tag:demo" record))
     (should (elogcat-tests--query-matches "package=:com.example.app" record))
     (should (elogcat-tests--query-matches "process~:worker$" record))
     (should (elogcat-tests--query-matches "message:'Hello World'" record))
     (setf (elogcat-record-message-group record)
           (elogcat--new-message-group "Main.kt"))
     (should (elogcat-tests--query-matches "message~:\"Main\\.kt\"" record))
     (should (elogcat-tests--query-matches "-tag:other" record))
     (should-not (elogcat-tests--query-matches "-tag:demo" record))
     (should-not (elogcat-tests--query-matches "tag=:demo" record)))))

(ert-deftest elogcat-query-uses-studio-implicit-grouping ()
  "Positive terms sharing a field OR while different fields AND."
  (elogcat-tests--with-buffer
   (let ((record (elogcat-tests--query-record)))
     (should (elogcat-tests--query-matches
              "tag:other tag:Demo package:example" record))
     (should-not (elogcat-tests--query-matches
                  "tag:other tag:missing package:example" record))
     (should-not (elogcat-tests--query-matches
                  "tag:Demo package:missing" record)))))

(ert-deftest elogcat-query-honors-operators-and-parentheses ()
  "Explicit AND binds tighter than OR and parentheses override precedence."
  (elogcat-tests--with-buffer
   (let ((record (elogcat-tests--query-record)))
     (should (elogcat-tests--query-matches
              "tag:missing | tag:Demo & package:example" record))
     (should-not (elogcat-tests--query-matches
                  "(tag:missing | tag:Demo) & package:other" record)))))

(ert-deftest elogcat-query-supports-empty-parens-and-assert-letters ()
  "Empty parentheses match all and F/A both represent Studio ASSERT."
  (elogcat-tests--with-buffer
   (let ((record (elogcat-tests--query-record :level "F")))
     (should (elogcat-tests--query-matches "()" record))
     (should (elogcat-tests--query-matches "is:assert" record))
     (should (elogcat-tests--query-matches "level:assert" record)))))

(ert-deftest elogcat-query-matches-complete-message-groups ()
  "A message term matching one stack line keeps every line in its group."
  (elogcat-tests--with-buffer
   (let* ((header (elogcat--parse-record
                   "09-18 12:34:58.000  1234  1234 E Demo: Failure"))
          (frame (elogcat--parse-record
                  "09-18 12:34:58.000  1234  1234 E Demo:     at demo.Main.run(Main.kt:42)"
                  header))
          (predicate (elogcat--query-compile "message:Main.kt")))
     (setq elogcat--query-predicate predicate)
     (should (elogcat--query-matches-p header))
     (should (elogcat--query-matches-p frame)))))

(ert-deftest elogcat-query-supports-level-age-and-is-filters ()
  "Level, age, crash, stacktrace, Firebase, and exact-level filters match Studio."
  (elogcat-tests--with-buffer
   (let* ((now (format-time-string "%m-%d %H:%M:%S.000"))
          (record (elogcat-tests--query-record))
          (crash (elogcat-tests--query-record
                  :level "E" :tag "AndroidRuntime"
                  :message "FATAL EXCEPTION: main"
                  :message-group
                  (elogcat--new-message-group "FATAL EXCEPTION: main")))
          (stack (elogcat-tests--query-record
                  :message "Failure"
                  :message-group
                  (elogcat--new-message-group
                   "Failure\n    at demo.Main.run(Main.kt:42)\n"))))
     (setf (elogcat-record-timestamp record) now)
     (should (elogcat-tests--query-matches "level:info" record))
     (should-not (elogcat-tests--query-matches "level:error" record))
     (should (elogcat-tests--query-matches "is:warn" record))
     (should (elogcat-tests--query-matches "age:10s" record))
     (should (elogcat-tests--query-matches "is:crash" crash))
     (should (elogcat-tests--query-matches "is:stacktrace" stack))
     (setf (elogcat-record-tag record) "FA")
     (should (elogcat-tests--query-matches "is:firebase" record)))))

(ert-deftest elogcat-query-supports-package-mine-and-match-case ()
  "Package mine uses the selected package and matching defaults to case-folded."
  (elogcat-tests--with-buffer
   (let ((record (elogcat-tests--query-record)))
     (setq elogcat-package-filter "com.example.app")
     (should (elogcat-tests--query-matches "package:mine" record))
     (should (elogcat-tests--query-matches "tag:demotag" record))
     (setq elogcat-query-match-case t)
     (should-not (elogcat-tests--query-matches "tag:demotag" record)))))

(ert-deftest elogcat-query-invalid-expression-falls-back-to-line-text ()
  "Invalid structured queries become a whole-line contains filter like Studio."
  (elogcat-tests--with-buffer
   (let ((record
          (elogcat-tests--query-record
           :message "level:nope"
           :message-group (elogcat--new-message-group "level:nope"))))
     (should (elogcat-tests--query-matches "level:nope" record))
     (should-not (elogcat-tests--query-matches "age:bogus" record)))))

(ert-deftest elogcat-query-completion-provides-static-context-values ()
  "Query completion switches candidates based on the field at point."
  (with-temp-buffer
    (insert "lev")
    (let ((capf (elogcat-query-completion-at-point)))
      (should (member "level:" (nth 2 capf))))
    (erase-buffer)
    (insert "level:w")
    (let ((capf (elogcat-query-completion-at-point)))
      (should (= (nth 0 capf) (+ 2 (string-match ":" (buffer-string)))))
      (should (member "warn" (nth 2 capf))))
    (erase-buffer)
    (insert "is:")
    (should (member "stacktrace"
                    (nth 2 (elogcat-query-completion-at-point))))
    (erase-buffer)
    (insert "age:")
    (should (member "10m"
                    (nth 2 (elogcat-query-completion-at-point))))))

(ert-deftest elogcat-query-completion-uses-backlog-values ()
  "Package, tag, and process candidates come from the current backlog."
  (elogcat-tests--with-buffer
   (setq elogcat--records (list (elogcat-tests--query-record)))
   (let ((elogcat--query-completion-source-buffer (current-buffer)))
     (with-temp-buffer
       (let ((elogcat--query-completion-source-buffer
              elogcat--query-completion-source-buffer))
         (insert "package:")
         (let ((candidates (nth 2 (elogcat-query-completion-at-point))))
           (should (member "mine" candidates))
           (should (member "com.example.app" candidates)))
         (erase-buffer)
         (insert "tag:")
         (should (member "DemoTag"
                         (nth 2 (elogcat-query-completion-at-point))))
         (erase-buffer)
         (insert "process:")
         (should (member "com.example.app:worker"
                         (nth 2 (elogcat-query-completion-at-point)))))))))

(ert-deftest elogcat-query-minibuffer-binds-tab-to-capf ()
  "The query minibuffer binds TAB directly to completion-at-point."
  (with-temp-buffer
    (elogcat--query-minibuffer-setup)
    (should (eq (local-key-binding (kbd "TAB"))
                #'completion-at-point))))

(ert-deftest elogcat-query-command-redraws-without-restarting ()
  "Setting a query changes only the backlog projection."
  (elogcat-tests--with-buffer
   (let ((record (elogcat-tests--query-record)))
     (setq elogcat--records (list record))
     (cl-letf (((symbol-function 'elogcat-stop)
                (lambda () (ert-fail "query restarted Logcat")))
               ((symbol-function 'message) #'ignore))
       (elogcat-set-query-filter "tag:Demo")
       (should (equal elogcat-query-filter "tag:Demo"))
       (should (string-match-p "Hello World" (buffer-string)))
       (elogcat-set-query-filter "tag:missing")
       (should (string-empty-p (buffer-string)))))))

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
           elogcat--query-predicate (elogcat--query-compile "tag:DemoTag"))
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
   (elogcat-set-query-filter "message:missing")
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
       (elogcat-set-query-filter "message:missing"))
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

(ert-deftest elogcat-is-a-specialized-major-mode ()
  "Logcat uses a read-only major mode derived from `special-mode'."
  (elogcat-tests--with-buffer
   (should (derived-mode-p 'elogcat-mode))
   (should buffer-read-only)))

(ert-deftest elogcat-mode-line-distinguishes-live-hold-and-paused ()
  "The mode line reports whether the visible stream follows its tail."
  (elogcat-tests--with-buffer
   (let ((buffer-read-only nil))
     (insert "one\ntwo\n"))
   (goto-char (point-max))
   (setq elogcat-stream-state 'live)
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
  "The mode map keeps all primary Logcat commands directly accessible."
  (dolist (binding '(("SPC" . elogcat-toggle-pause)
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
                     ("P" . elogcat-select-mine)))
    (should (eq (lookup-key elogcat-mode-map (kbd (car binding)))
                (cdr binding))))
  (should (eq (lookup-key elogcat-mode-map [remap next-line])
              #'elogcat-next-occurrence))
  (should (eq (lookup-key elogcat-mode-map [remap previous-line])
              #'elogcat-previous-occurrence)))

(ert-deftest elogcat-dispatch-is-transient-prefix ()
  "The mode menu entry exposes the primary Transient commands."
  (should (commandp #'elogcat-dispatch))
  (dolist (key '("SPC" "/" "l" "P" "V" "D" "q"))
    (should (transient-get-suffix 'elogcat-dispatch key))))

(ert-deftest elogcat-device-parser-and-adb-command-use-serial ()
  "Device discovery preserves model labels and all adb calls use the serial."
  (elogcat-tests--with-buffer
   (let ((devices (elogcat--parse-devices
                   "List of devices attached\nSER1 device product:p model:Pixel_9 device:x\nSER2 offline\n")))
     (should (equal devices '(("SER1" . "Pixel_9"))))
     (setq elogcat-device-serial "SER1")
     (should (equal (elogcat--adb-command "shell" "ps")
                    '("adb" "-s" "SER1" "shell" "ps"))))))

(ert-deftest elogcat-package-cache-parses-uid-metadata ()
  "Installed package metadata is reusable independently of process output."
  (let ((metadata (elogcat--parse-package-output
                   "package:com.example uid:10123\npackage:com.other uid:10124\n")))
    (should (equal (sort (plist-get metadata :packages) #'string<)
                   '("com.example" "com.other")))
    (should (equal (gethash "10123" (plist-get metadata :uids))
                   '("com.example")))))

(ert-deftest elogcat-query-diagnostics-preserve-fallback ()
  "Invalid queries remain text filters while exposing a diagnostic."
  (elogcat-tests--with-buffer
   (let ((term (elogcat--query-compile "tag~:[")))
     (should (elogcat-query-term-p term))
     (should elogcat-query-error)
     (should (equal (elogcat-query-term-field term) 'implicit)))))

(ert-deftest elogcat-structured-display-and-folding-only-change-projection ()
  "Column presets and exception folding redraw without changing records."
  (elogcat-tests--with-buffer
   (let* ((header (elogcat--parse-record
                   "09-18 12:34:58.000  1234  1234 E Demo: Failure"))
          (frame (elogcat--parse-record
                  "    at demo.Main.run(Main.kt:42)" header)))
     (setq elogcat--records (list header frame)
           elogcat-visible-fields '(level tag message))
     (elogcat--render-backlog)
     (should (string-match-p "E Demo Failure" (buffer-string)))
     (goto-char (point-max))
     (forward-line -1)
     (elogcat-toggle-exception-fold)
     (should (= (length elogcat--records) 2))
     (should (string-match-p "stack frames folded" (buffer-string)))
     (should-not (string-match-p "Main.kt:42" (buffer-string))))))

(ert-deftest elogcat-clear-is-asynchronous-and-device-scoped ()
  "Clearing starts a serial-qualified process without sleeping."
  (elogcat-tests--with-buffer
   (setq elogcat-device-serial "SER1")
   (let (process-command)
     (cl-letf (((symbol-function 'make-process)
                (lambda (&rest arguments)
                  (setq process-command (plist-get arguments :command))
                  'fake-process))
               ((symbol-function 'process-put) #'ignore)
               ((symbol-function 'process-live-p) (lambda (_process) nil))
               ((symbol-function 'sleep-for)
                (lambda (&rest _) (ert-fail "clear blocked Emacs"))))
       (elogcat-erase-buffer)
       (should (equal (seq-take process-command 4)
                      '("adb" "-s" "SER1" "shell")))))))

(ert-deftest elogcat-visit-source-opens-project-file-at-line ()
  "Stack-frame navigation resolves source files inside the project root."
  (let ((root (make-temp-file "elogcat-project" t)))
    (unwind-protect
        (let* ((file (expand-file-name "src/Main.kt" root))
               (elogcat-project-root root))
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert "one\ntwo\nthree\n"))
          (elogcat-tests--with-buffer
           (setq elogcat-project-root root)
           (let ((buffer-read-only nil))
             (insert (elogcat--format-record
                      (elogcat--parse-record
                       "    at demo.Main.run(Main.kt:2)"))))
           (goto-char (point-min))
           (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
             (elogcat-visit-source))
           (should (equal (buffer-file-name) file))
           (should (= (line-number-at-pos) 2))
           (kill-buffer (current-buffer))))
      (delete-directory root t))))

(ert-deftest elogcat-reconnect-cancels-pending-retry ()
  "A manual reconnect cancels its pending automatic retry."
  (elogcat-tests--with-buffer
   (let ((elogcat--reconnect-timer 'retry-timer)
         canceled (started 0))
     (cl-letf (((symbol-function 'timerp) (lambda (timer) (eq timer 'retry-timer)))
               ((symbol-function 'cancel-timer) (lambda (timer) (setq canceled timer)))
               ((symbol-function 'process-live-p) (lambda (_process) nil))
               ((symbol-function 'elogcat--stop-process-monitor) #'ignore)
               ((symbol-function 'elogcat--start-stream) (lambda () (cl-incf started))))
       (elogcat-reconnect)
       (should (eq canceled 'retry-timer))
       (should-not elogcat--reconnect-timer)
       (should (= started 1))))))

(ert-deftest elogcat-old-device-package-result-is-discarded ()
  "A package query cannot populate cache after the selected serial changes."
  (elogcat-tests--with-buffer
   (let ((process 'old-query)
         (output (generate-new-buffer " *elogcat-old-packages*"))
         callback-value)
     (unwind-protect
         (progn
           (with-current-buffer output
             (insert "package:com.old uid:10123\n"))
           (setq elogcat--package-refresh-process process
                 elogcat--package-refresh-callbacks
                 (list (lambda (metadata) (setq callback-value metadata)))
                 elogcat-device-serial "NEW")
           (cl-letf (((symbol-function 'process-get)
                      (lambda (_process property)
                        (and (eq property 'elogcat-device-serial) "OLD")))
                     ((symbol-function 'process-status) (lambda (_) 'exit))
                     ((symbol-function 'process-exit-status) (lambda (_) 0)))
             (elogcat--finish-package-query process output))
           (should-not callback-value)
           (should-not (gethash "NEW" elogcat--package-cache))
           (should-not (gethash "OLD" elogcat--package-cache)))
       (kill-buffer output)))))

(ert-deftest elogcat-unresolved-record-survives-unrelated-process-refresh ()
  "A later process refresh can enrich a PID absent from the first refresh."
  (elogcat-tests--with-buffer
   (let* ((record (elogcat--parse-record elogcat-tests--debug))
          (empty (make-hash-table :test #'equal))
          (resolved (make-hash-table :test #'equal)))
     (setq elogcat--unresolved-records (list record))
     (elogcat--update-process-table empty)
     (should (equal elogcat--unresolved-records (list record)))
     (puthash "1234"
              (make-elogcat-process-info
               :application-ids '("com.example") :process-name "com.example")
              resolved)
     (elogcat--update-process-table resolved)
     (should-not elogcat--unresolved-records)
     (should (equal (elogcat-record-application-ids record) '("com.example"))))))

(ert-deftest elogcat-clearing-query-clears-parser-diagnostic ()
  "Clearing an invalid query removes its visible parser diagnostic."
  (elogcat-tests--with-buffer
   (elogcat-set-query-filter "tag~:[")
   (should elogcat-query-error)
   (elogcat-set-query-filter "")
   (should-not elogcat-query-error)
   (should-not elogcat-query-filter)))

(ert-deftest elogcat-stop-invalidates-pending-device-discovery ()
  "Stopping prevents a late device-discovery sentinel from starting Logcat."
  (elogcat-tests--with-buffer
   (let ((elogcat--device-query-process 'discovery)
         deleted)
     (cl-letf (((symbol-function 'process-live-p)
                (lambda (process) (eq process 'discovery)))
               ((symbol-function 'delete-process)
                (lambda (process) (setq deleted process)))
               ((symbol-function 'elogcat--stop-process-monitor) #'ignore))
       (elogcat-stop)
       (should (eq deleted 'discovery))
       (should-not elogcat--device-query-process)))))

(ert-deftest elogcat-stale-clear-result-cannot-restart-current-session ()
  "A superseded clear sentinel cannot stop or restart the current stream."
  (elogcat-tests--with-buffer
   (let ((old-clear 'old-clear)
         (elogcat--clear-process 'new-clear)
         (elogcat-device-serial "NEW")
         stopped restarted)
     (cl-letf (((symbol-function 'process-get)
                (lambda (_process property)
                  (pcase property
                    ('elogcat-target-buffer (current-buffer))
                    ('elogcat-device-serial "OLD"))))
               ((symbol-function 'process-status) (lambda (_) 'exit))
               ((symbol-function 'process-exit-status) (lambda (_) 0))
               ((symbol-function 'elogcat-stop) (lambda () (setq stopped t)))
               ((symbol-function 'elogcat) (lambda (&rest _) (setq restarted t))))
       (elogcat--clear-finished old-clear "finished")
       (should-not stopped)
       (should-not restarted)
       (should (eq elogcat--clear-process 'new-clear))))))

(ert-deftest elogcat-stale-stream-output-is-ignored ()
  "Output from a replaced stream cannot enter the current backlog."
  (elogcat-tests--with-buffer
   (setq elogcat--stream-process 'current-stream
         elogcat-stream-state 'connecting)
   (elogcat-process-filter 'old-stream (concat elogcat-tests--debug "\n"))
   (should-not elogcat--records)
   (should (eq elogcat-stream-state 'connecting))))

(ert-deftest elogcat-stale-stream-exit-does-not-change-current-state ()
  "The sentinel for a replaced stream cannot stop the current session."
  (elogcat-tests--with-buffer
   (setq elogcat--stream-process 'current-stream
         elogcat-stream-state 'live)
   (let (reconnected)
     (cl-letf (((symbol-function 'process-buffer)
                (lambda (_process) (current-buffer)))
               ((symbol-function 'process-live-p) (lambda (_process) nil))
               ((symbol-function 'elogcat--schedule-reconnect)
                (lambda () (setq reconnected t))))
       (elogcat-process-sentinel 'old-stream "finished\n")
       (should (eq elogcat--stream-process 'current-stream))
       (should (eq elogcat-stream-state 'live))
       (should-not reconnected)))))

(ert-deftest elogcat-stream-status-reports-offline-and-error ()
  "Connection state replaces misleading LIVE status when disconnected."
  (elogcat-tests--with-buffer
   (setq elogcat-stream-state 'offline)
   (should (string-match-p "OFFLINE" (elogcat-make-status)))
   (setq elogcat-stream-state 'error)
   (should (string-match-p "ERROR" (elogcat-make-status)))))

(ert-deftest elogcat-transient-layout-is-stable ()
  "Reloading features does not replace user-added menu suffixes."
  (unwind-protect
      (progn
        (transient-append-suffix
         'elogcat-dispatch "q"
         '("Z" "Test customization" ignore))
        (require 'elogcat)
        (should (transient-get-suffix 'elogcat-dispatch "Z")))
    (ignore-errors
      (transient-remove-suffix 'elogcat-dispatch "Z"))))

(ert-deftest elogcat-mode-defaults-to-package-mine ()
  "New Logcat buffers use Android Studio's package:mine filter."
  (let ((elogcat-tests--default-query "package:mine"))
    (elogcat-tests--with-buffer
     (should (equal elogcat-query-filter "package:mine"))
     (should elogcat--query-predicate)
     (should (string-match-p "package:mine" (elogcat--make-header-line))))))

;;; elogcat-tests.el ends here
