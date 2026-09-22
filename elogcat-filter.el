;;; elogcat-filter.el --- Studio-compatible filters for elogcat  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Youngjoo Lee

;;; Commentary:

;; Android Studio compatible filter parser, matcher, and completion.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'elogcat-core)

(cl-defstruct elogcat-query-term
  "One leaf in an Android Studio compatible filter expression."
  field operator value negated)

(defconst elogcat--query-key-candidates
  '("tag:" "tag=:" "tag~:" "-tag:" "-tag=:" "-tag~:"
    "package:" "package=:" "package~:" "-package:" "-package=:" "-package~:"
    "process:" "process=:" "process~:" "-process:" "-process=:" "-process~:"
    "message:" "message=:" "message~:" "-message:" "-message=:" "-message~:"
    "line:" "line=:" "line~:" "-line:" "-line=:" "-line~:"
    "level:" "age:" "is:" "name:")
  "Top-level Android Studio filter completion candidates.")

(defun elogcat--query-tokenize (query)
  "Tokenize Android Studio filter QUERY."
  (let ((index 0) (length (length query)) tokens)
    (while (< index length)
      (cond
       ((memq (aref query index) '(?\s ?\t ?\n))
        (cl-incf index))
       ((memq (aref query index) '(?& ?| ?\( ?\)))
        (push (pcase (aref query index)
                (?& 'and) (?| 'or) (?\( 'lparen) (?\) 'rparen))
              tokens)
        (cl-incf index))
       (t
        (let (characters quote)
          (while (and (< index length)
                      (or quote
                          (not (memq (aref query index)
                                     '(?\s ?\t ?\n ?& ?| ?\( ?\))))))
            (let ((character (aref query index)))
              (cond
               ((and (= character ?\\) (< (1+ index) length))
                (let ((next (aref query (1+ index))))
                  (if (or (and quote (memq next (list quote ?\\)))
                          (and (null quote)
                               (memq next
                                     '(?\s ?\t ?& ?| ?\( ?\) ?\\ ?' ?\"))))
                      (push next characters)
                    (push character characters)
                    (push next characters))
                  (cl-incf index 2)))
               ((memq character '(?' ?\"))
                (cond ((null quote) (setq quote character))
                      ((= quote character) (setq quote nil))
                      (t (push character characters)))
                (cl-incf index))
               (t (push character characters) (cl-incf index)))))
          (when quote (error "Unterminated quote"))
          (push (apply #'string (nreverse characters)) tokens)))))
    (nreverse tokens)))

(defconst elogcat--query-fields
  '("tag" "package" "process" "message" "line")
  "Field names supported by Android Studio Logcat filters.")

(defun elogcat--query-normalize-tokens (tokens)
  "Merge separated field keys and values in TOKENS."
  (let (normalized)
    (while tokens
      (let ((token (pop tokens)))
        (if (and (stringp token)
                 (string-match-p "\\(?:~\\|=\\)?:\\'" token)
                 (stringp (car tokens)))
            (push (concat token (pop tokens)) normalized)
          (push token normalized))))
    (nreverse normalized)))

(defun elogcat--query-level (value)
  "Return the priority letter represented by VALUE, or nil."
  (cdr (assoc-string
        (downcase value)
        '(("verbose" . "V") ("v" . "V") ("debug" . "D") ("d" . "D")
          ("info" . "I") ("i" . "I") ("warn" . "W") ("warning" . "W")
          ("w" . "W") ("error" . "E") ("e" . "E") ("fatal" . "F")
          ("f" . "F") ("assert" . "A") ("a" . "A")))))

(defun elogcat--query-term (token)
  "Parse TOKEN into an `elogcat-query-term'."
  (unless (stringp token) (error "Expected filter term"))
  (let ((case-fold-search nil))
    (if (string-match
         "\\`\\(-?\\)\\([[:alpha:]]+\\)\\(~\\|=\\)?:\\(.*\\)\\'" token)
        (let* ((negated (not (string-empty-p (match-string 1 token))))
               (field (downcase (match-string 2 token)))
               (operator (pcase (match-string 3 token)
                           ("~" 'regex) ("=" 'exact) (_ 'contains)))
               (value (match-string 4 token)))
          (unless (or (member field elogcat--query-fields)
                      (member field '("level" "age" "is" "name")))
            (error "Invalid filter field: %s" field))
          (make-elogcat-query-term :field (intern field) :operator operator
                                   :value value :negated negated))
      (make-elogcat-query-term :field 'implicit :operator 'contains
                               :value token :negated nil))))

(defun elogcat--query-group-key (term index)
  "Return implicit-OR grouping key for TERM at INDEX."
  (let ((field (elogcat-query-term-field term)))
    (cond
     ((elogcat-query-term-negated term) (cons 'unique index))
     ((eq field 'implicit) (cons 'unique index))
     ((memq field '(tag package process message line level age is)) field)
     (t (cons 'unique index)))))

(defun elogcat--query-implicit-ast (tokens)
  "Build Android Studio's implicit same-field OR tree from TOKENS."
  (let ((groups nil) (index 0))
    (dolist (token tokens)
      (unless (stringp token) (error "Unexpected filter operator"))
      (let* ((term (elogcat--query-term token))
             (key (elogcat--query-group-key term index))
             (entry (assq key groups)))
        (if entry
            (setcdr entry (append (cdr entry) (list term)))
          (setq groups (append groups (list (list key term))))))
      (cl-incf index))
    (let ((nodes (mapcar (lambda (group)
                           (let ((terms (cdr group)))
                             (if (= (length terms) 1)
                                 (car terms)
                               (cons 'or terms))))
                         groups)))
      (if (= (length nodes) 1) (car nodes) (cons 'and nodes)))))

(defvar elogcat--query-parser-tokens nil)

(defun elogcat--query-parse-primary ()
  "Parse one primary expression from `elogcat--query-parser-tokens'."
  (let ((token (pop elogcat--query-parser-tokens)))
    (cond
     ((eq token 'lparen)
      (if (eq (car elogcat--query-parser-tokens) 'rparen)
          (progn (pop elogcat--query-parser-tokens) 'true)
        (let ((expression (elogcat--query-parse-or)))
          (unless (eq (pop elogcat--query-parser-tokens) 'rparen)
            (error "Missing closing parenthesis"))
          expression)))
     ((stringp token) (elogcat--query-term token))
     (t (error "Expected filter term")))))

(defun elogcat--query-parse-and ()
  "Parse an AND expression from `elogcat--query-parser-tokens'."
  (let ((nodes (list (elogcat--query-parse-primary))))
    (while (or (eq (car elogcat--query-parser-tokens) 'and)
               (stringp (car elogcat--query-parser-tokens))
               (eq (car elogcat--query-parser-tokens) 'lparen))
      (when (eq (car elogcat--query-parser-tokens) 'and)
        (pop elogcat--query-parser-tokens))
      (push (elogcat--query-parse-primary) nodes))
    (setq nodes (nreverse nodes))
    (if (= (length nodes) 1) (car nodes) (cons 'and nodes))))

(defun elogcat--query-parse-or ()
  "Parse an OR expression from `elogcat--query-parser-tokens'."
  (let ((nodes (list (elogcat--query-parse-and))))
    (while (eq (car elogcat--query-parser-tokens) 'or)
      (pop elogcat--query-parser-tokens)
      (push (elogcat--query-parse-and) nodes))
    (setq nodes (nreverse nodes))
    (if (= (length nodes) 1) (car nodes) (cons 'or nodes))))

(defun elogcat--query-parse (query)
  "Return an AST for Android Studio filter QUERY."
  (let* ((tokens (elogcat--query-normalize-tokens
                  (elogcat--query-tokenize query)))
         (explicit (seq-some (lambda (token)
                               (memq token '(and or lparen rparen)))
                             tokens)))
    (if (null tokens)
        nil
      (if (not explicit)
          (elogcat--query-implicit-ast tokens)
        (let ((elogcat--query-parser-tokens tokens))
          (prog1 (elogcat--query-parse-or)
            (when elogcat--query-parser-tokens
              (error "Unexpected filter token"))))))))

(defun elogcat--query-line-text (record)
  "Return Android Studio style searchable line text for RECORD."
  (mapconcat #'identity
             (delq nil (list (elogcat-record-timestamp record)
                             (elogcat-record-pid record)
                             (elogcat-record-tid record)
                             (elogcat-record-level record)
                             (elogcat-record-tag record)
                             (elogcat-record-process-name record)
                             (string-join (elogcat-record-application-ids record) " ")
                             (elogcat--message-text record)))
             " "))

(defun elogcat--query-field-values (field record)
  "Return FIELD values from RECORD."
  (pcase field
    ('tag (list (or (elogcat-record-tag record) "")))
    ('package (or (elogcat-record-application-ids record) '("")))
    ('process (list (or (elogcat-record-process-name record) "")))
    ('message (list (or (elogcat--message-text record) "")))
    ((or 'line 'implicit) (list (elogcat--query-line-text record)))
    (_ nil)))

(defun elogcat--query-string-match-p (operator pattern value)
  "Return whether VALUE matches PATTERN using OPERATOR."
  (let ((case-fold-search (not elogcat-query-match-case)))
    (pcase operator
      ('contains (string-match-p (regexp-quote pattern) value))
      ('exact (string-equal (if case-fold-search (downcase pattern) pattern)
                            (if case-fold-search (downcase value) value)))
      ('regex (string-match-p pattern value))
      (_ nil))))

(defun elogcat--query-level-index (level)
  "Return Android Studio priority index for LEVEL."
  (or (cl-position (if (equal level "F") "A" level)
                   '("V" "D" "I" "W" "E" "A") :test #'string=)
      -1))

(defconst elogcat--firebase-tags
  '("AppInstallOperation" "AppInviteActivity" "AppInviteAgent"
    "AppInviteAnalytics" "AppInviteLogger" "BackgroundTask" "ClassMapper"
    "Connection" "DataOperation" "EventRaiser" "FA" "FirebaseAppIndex"
    "FirebaseDatabase" "FirebaseInstanceId" "FirebaseMessaging"
    "FirebaseRemoteConfig" "NetworkRequest" "Persistence"
    "PersistentConnection" "RepoOperation" "RunLoop" "StorageTask"
    "SyncTree" "Transaction" "WebSocket")
  "Tags recognized by Android Studio's is:firebase filter.")

(defun elogcat--query-age-seconds (value)
  "Return VALUE converted from Android Studio age syntax to seconds."
  (unless (string-match "\\`\\([0-9]+\\)\\([smhd]\\)\\'" value)
    (error "Invalid age: %s" value))
  (* (string-to-number (match-string 1 value))
     (pcase (match-string 2 value)
       ("s" 1) ("m" 60) ("h" 3600) ("d" 86400))))

(defun elogcat--query-record-time (record)
  "Return RECORD time as the closest matching year, or nil."
  (when-let* ((timestamp (elogcat-record-timestamp record)))
    (let* ((current-year (string-to-number (format-time-string "%Y")))
           (candidates
            (delq nil
                  (mapcar (lambda (year)
                            (ignore-errors
                              (date-to-time (format "%d-%s" year timestamp))))
                          (list (1- current-year) current-year
                                (1+ current-year))))))
      (car (sort candidates
                 (lambda (left right)
                   (< (abs (float-time (time-subtract nil left)))
                      (abs (float-time (time-subtract nil right))))))))))

(defun elogcat--query-special-match-p (term record)
  "Match special query TERM against RECORD."
  (let ((field (elogcat-query-term-field term))
        (value (downcase (elogcat-query-term-value term))))
    (pcase field
      ('level
       (when-let* ((required (elogcat--query-level value)))
         (>= (elogcat--query-level-index (elogcat-record-level record))
             (elogcat--query-level-index required))))
      ('age
       (when-let* ((time (elogcat--query-record-time record)))
         (<= (float-time (time-subtract nil time))
             (elogcat--query-age-seconds value))))
      ('name t)
      ('is
       (cond
        ((equal value "crash")
         (or (and (equal (elogcat-record-level record) "E")
                  (equal (elogcat-record-tag record) "AndroidRuntime")
                  (string-prefix-p "FATAL EXCEPTION"
                                   (elogcat--message-text record)))
             (and (equal (elogcat-record-level record) "A")
                  (member (elogcat-record-tag record) '("DEBUG" "libc")))))
        ((equal value "stacktrace")
         (string-match-p "\n[[:space:]]*at .+(.+)\n?"
                         (elogcat--message-text record)))
        ((equal value "firebase")
         (member (elogcat-record-tag record) elogcat--firebase-tags))
        ((elogcat--query-level value)
         (= (elogcat--query-level-index (elogcat-record-level record))
            (elogcat--query-level-index (elogcat--query-level value))))
        (t (error "Invalid is filter: %s" value))))
      (_ nil))))

(defun elogcat--query-term-match-p (term record)
  "Return non-nil when query TERM matches RECORD."
  (let* ((field (elogcat-query-term-field term))
         (value (elogcat-query-term-value term))
         (matched
          (if (memq field '(level age is name))
              (elogcat--query-special-match-p term record)
            (if (and (eq field 'package) (equal value "mine"))
                (and elogcat-package-filter
                     (elogcat--package-matches-p record))
              (seq-some
               (lambda (field-value)
                 (elogcat--query-string-match-p
                  (elogcat-query-term-operator term) value field-value))
               (elogcat--query-field-values field record))))))
    (if (elogcat-query-term-negated term) (not matched) matched)))

(defun elogcat--query-ast-match-p (ast record)
  "Return non-nil when AST matches RECORD."
  (if (eq ast 'true)
      t
    (pcase (car-safe ast)
      ('and (seq-every-p (lambda (node)
                           (elogcat--query-ast-match-p node record))
                         (cdr ast)))
      ('or (seq-some (lambda (node)
                       (elogcat--query-ast-match-p node record))
                     (cdr ast)))
      (_ (elogcat--query-term-match-p ast record)))))

(defun elogcat--query-validate-term (term)
  "Validate query TERM or signal an error."
  (pcase (elogcat-query-term-field term)
    ('level (unless (elogcat--query-level (elogcat-query-term-value term))
              (error "Invalid level")))
    ('age (elogcat--query-age-seconds (elogcat-query-term-value term)))
    ('is (let ((value (downcase (elogcat-query-term-value term))))
           (unless (or (member value '("crash" "firebase" "stacktrace"))
                       (elogcat--query-level value))
             (error "Invalid is filter"))))
    (_ (when (eq (elogcat-query-term-operator term) 'regex)
         (string-match-p (elogcat-query-term-value term) "")))))

(defun elogcat--query-walk-terms (ast function)
  "Call FUNCTION for every leaf term in AST."
  (if (eq ast 'true)
      nil
    (if (memq (car-safe ast) '(and or))
        (dolist (node (cdr ast))
          (elogcat--query-walk-terms node function))
      (funcall function ast))))

(defun elogcat--query-compile (query)
  "Compile QUERY, recording diagnostics and falling back to text on errors."
  (setq elogcat-query-error nil)
  (unless (string-empty-p query)
    (condition-case error-data
        (let ((ast (elogcat--query-parse query)))
          (elogcat--query-walk-terms ast #'elogcat--query-validate-term)
          ast)
      (error
       (setq elogcat-query-error (error-message-string error-data))
       (make-elogcat-query-term :field 'implicit :operator 'contains
                                :value query :negated nil)))))

(defun elogcat--query-matches-p (record)
  "Return non-nil when RECORD passes `elogcat-query-filter'."
  (or (null elogcat--query-predicate)
      (elogcat--query-ast-match-p elogcat--query-predicate record)))

(defun elogcat--query-record-values (accessor)
  "Return distinct non-empty values from ACCESSOR over the source backlog."
  (when (buffer-live-p elogcat--query-completion-source-buffer)
    (with-current-buffer elogcat--query-completion-source-buffer
      (delete-dups
       (delq nil
             (cl-loop for record in elogcat--records
                      append
                      (let ((value (funcall accessor record)))
                        (cond ((listp value) value)
                              ((and value (not (string-empty-p value)))
                               (list value))))))))))

(defun elogcat--query-completion-context ()
  "Return completion field, prefix start, and prefix at point."
  (let* ((end (point))
         (start (save-excursion
                  (skip-chars-backward "^ \t\n&|()")
                  (point)))
         (token (buffer-substring-no-properties start end)))
    (if (string-match
         "\\`-?\\([[:alpha:]]+\\)\\(?:~\\|=\\)?:\\(.*\\)\\'" token)
        (list (intern (downcase (match-string 1 token)))
              (+ start (match-beginning 2))
              (match-string 2 token))
      (list 'key start token))))

(defun elogcat--query-completion-candidates (field)
  "Return filter completion candidates for FIELD."
  (pcase field
    ('key elogcat--query-key-candidates)
    ('level '("verbose" "debug" "info" "warn" "error" "assert"))
    ('is '("crash" "stacktrace" "firebase" "verbose" "debug" "info"
           "warn" "error" "assert"))
    ('age '("10s" "30s" "1m" "5m" "10m" "30m" "1h" "6h" "1d"))
    ('package
     (cons "mine"
           (elogcat--query-record-values
            #'elogcat-record-application-ids)))
    ('tag
     (elogcat--query-record-values
      (lambda (record) (elogcat-record-tag record))))
    ('process
     (elogcat--query-record-values
      (lambda (record) (elogcat-record-process-name record))))
    (_ nil)))

(defun elogcat-query-completion-at-point ()
  "Complete Android Studio Logcat filter syntax at point."
  (pcase-let ((`(,field ,start ,_prefix)
               (elogcat--query-completion-context)))
    (when-let* ((candidates (elogcat--query-completion-candidates field)))
      (list start (point) candidates
            :exclusive 'no
            :annotation-function
            (lambda (_candidate)
              (pcase field
                ('key "  filter key")
                ('package "  application ID")
                ('tag "  observed tag")
                ('process "  observed process")
                ('level "  minimum level")
                ('is "  exact/special filter")
                ('age "  recent duration")
                (_ "")))))))

(defconst elogcat--query-font-lock-keywords
  '(("\\_<-?\\([[:alpha:]]+\\)\\(?:~\\|=\\)?:"
     (1 font-lock-keyword-face))
    ("[&|()]" . font-lock-builtin-face)
    ("\\_<\\(?:mine\\|crash\\|stacktrace\\|firebase\\)\\_>"
     . font-lock-constant-face))
  "Font-lock rules used while editing Logcat queries.")

(defun elogcat--query-minibuffer-setup ()
  "Install Logcat query completion and syntax highlighting."
  (add-hook 'completion-at-point-functions
            #'elogcat-query-completion-at-point nil t)
  (setq-local font-lock-defaults '(elogcat--query-font-lock-keywords))
  (font-lock-mode 1)
  (local-set-key (kbd "TAB") #'completion-at-point))

(defun elogcat--read-query-filter ()
  "Read a Logcat query with contextual completion."
  (let ((elogcat--query-completion-source-buffer (current-buffer))
        (minibuffer-setup-hook
         (cons #'elogcat--query-minibuffer-setup minibuffer-setup-hook)))
    (read-string "Logcat filter: " elogcat-query-filter
                 'elogcat-query-filter-history)))

(defun elogcat-set-query-filter (query)
  "Set Android Studio compatible filter QUERY and redraw the backlog."
  (interactive (list (elogcat--read-query-filter)))
  (setq elogcat-query-filter (unless (string-empty-p query) query)
        elogcat-query-error nil
        elogcat--query-predicate
        (and elogcat-query-filter
             (elogcat--query-compile elogcat-query-filter)))
  (funcall elogcat--redraw-function)
  (message "elogcat: query filter %s"
           (or elogcat-query-filter "cleared")))

(defun elogcat-toggle-query-match-case ()
  "Toggle case-sensitive matching for the structured query filter."
  (interactive)
  (setq elogcat-query-match-case (not elogcat-query-match-case))
  (funcall elogcat--redraw-function)
  (message "elogcat: query match case %s"
           (if elogcat-query-match-case "on" "off")))


(provide 'elogcat-filter)
;;; elogcat-filter.el ends here
