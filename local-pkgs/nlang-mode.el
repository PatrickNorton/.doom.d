(require 'cl-lib)

(defconst nlang-font-lock-keywords
      (let* (
             ;; define several categories of keywords
             (x-keywords '("if" "for" "elif" "else" "do" "func" "class" "method" "while" "in" "from"
                           "import" "export" "typeget" "dotimes" "break" "continue" "return"
                           "context" "lambda" "property" "enter" "exit" "try" "except" "finally"
                           "with" "as" "assert" "del" "yield" "raise" "typedef" "some" "interface"
                           "casted" "switch" "case" "enum" "default" "defer" "var" "sync" "nobreak"
                           "generic" "union"))
             (x-descriptors '("public" "private" "protected" "mut" "mref" "readonly" "const" "nonfinal" "final"
                              "pubget" "static" "generator" "synced" "native" "auto"))
             (x-types '("dec" "int" "list" "dict" "str" "set" "char" "bool" "bytes" "object" "slice" "tuple"
                        "type" "Array"))
             (x-builtins '("true" "false" "null" "self" "cls"))
             (x-functions '("sorted" "print" "open" "range" "id" "hash" "enumerate" "reversed" "input" "repr"
                            "iter"))
             (x-r-operators-list '("==" "!=" "<=>" "+" "-" "*" "/" "++" "--" "**" "//" "<" ">" "<=" ">="
                                   "<<" ">>" "&" "|" "^" "~" "%"))
             (x-r-operators (mapcan (lambda (arg) (list arg (concat "r" arg))) x-r-operators-list))
             (x-operators-list '("enter" "exit" "[]" "[]=" "[:]" "[:]=" "()" "u-" "iter" "iter[:]"
                                 "new" "in" "int" "missing" "hash" "reversed" "str" "repr" "bool"
                                 "del" "del[]"))
             (x-operators (append x-r-operators x-operators-list))
             (x-arith-ops '("->" "==" "!=" "<=>" "<" ">" "<=" ">=" "+" "-" "*" "/" "++" "--" "**" "//"
                            "??" "<<" ">>" "!!" "~" "%" "^" "&" "|"))
             (x-keyword-ops '("is" "instanceof"))
             (x-assign-ops '("=" ":="))
             (x-backslashed-ops (mapcar (lambda (arg) (concat "\\" arg))
                                        '("==" "!=" "<=>" "<" ">" "<=" ">=" "+" "-" "*" "/"
                                          "++" "--" "**" "//" "u-" "<<" ">>" "&" "|" "^" "~" "%"
                                          "and" "or" "xor" "not")))
             (x-bool-ops '("and" "or" "not" "xor"))
             (x-augmented-ops (mapcar (lambda (arg) (concat arg "="))
                                      '("+" "-" "*" "/" "**" "//" "%" "<<" ">>" "??" "&" "|" "^"
                                        "and" "or" "xor")))
             ;; generate regex string for each category of keywords
             (x-keywords-regexp (regexp-opt x-keywords 'symbols))
             (x-descriptors-regexp (regexp-opt x-descriptors 'symbols))
             (x-types-regexp (regexp-opt x-types 'symbols))
             (x-constants-regexp (regexp-opt x-builtins 'symbols))
             (x-functions-regexp (regexp-opt x-functions 'symbols))
             (x-operators-regexp (rx "operator" symbol-end (* space) (regexp (regexp-opt x-operators))))
             (x-invalid-op-regexp "operator")
             (x-arithmetic-regexp (regexp-opt x-arith-ops))
             (x-assign-regexp (regexp-opt x-assign-ops))
             (x-backslashed-regexp (regexp-opt x-backslashed-ops))
             (x-bool-regexp (regexp-opt x-bool-ops 'symbols))
             (x-augmented-regexp (regexp-opt x-augmented-ops 'symbols))
             (x-keyword-ops-regexp (regexp-opt x-keyword-ops 'symbols))
             (x-func-name-regexp (rx (or "func" "method") (+ blank) (group symbol-start (+? nonl) symbol-end)))
             (x-class-name-regexp (rx (or "class" "union" "enum" "interface")
                                      (+ blank) (group symbol-start (+? nonl) symbol-end)))
             (x-comment-regexp (rx ?# (not ?|) (* nonl)))
             (x-block-comment-regexp (rx "#|" (*? anychar) "|#"))
             (x-property-get-set-regexp (rx (group (or "get" "set")) (* blank) ?\{)))

        `(
          nlang-mode:font-lock-f-strings
          (,x-block-comment-regexp . font-lock-comment-face)
          (,x-comment-regexp . font-lock-comment-face)
          (,x-operators-regexp . font-lock-keyword-face)
          (,x-invalid-op-regexp . font-lock-warning-face)
          (,x-keyword-ops-regexp . font-lock-keyword-face)
          (,x-backslashed-regexp . font-lock-keyword-face)
          (,x-func-name-regexp . (1 font-lock-function-name-face))
          (,x-class-name-regexp . (1 font-lock-type-face))
          (,x-types-regexp . font-lock-type-face)
          (,x-descriptors-regexp . font-lock-keyword-face)
          (,x-constants-regexp . font-lock-constant-face)
          (,x-bool-regexp . font-lock-keyword-face)
          (,x-functions-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-property-get-set-regexp . (1 font-lock-keyword-face))
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

(defvar nlang-mode-syntax-table (make-syntax-table prog-mode-syntax-table))

(modify-syntax-entry ?# "< 14" nlang-mode-syntax-table)
(modify-syntax-entry ?| ". 2b3b" nlang-mode-syntax-table)
(modify-syntax-entry ?\s "-" nlang-mode-syntax-table)
(modify-syntax-entry ?\n ">" nlang-mode-syntax-table)
(modify-syntax-entry ?' "\"" nlang-mode-syntax-table)
(modify-syntax-entry ?\\ "\\" nlang-mode-syntax-table)
(modify-syntax-entry '(?a . ?z) "w" nlang-mode-syntax-table)
(modify-syntax-entry '(?A . ?Z) "w" nlang-mode-syntax-table)
(modify-syntax-entry ?_ "_" nlang-mode-syntax-table)

(dolist (chr '(?$ ?& ?* ?+ ?- ?< ?>))
  (modify-syntax-entry chr "." nlang-mode-syntax-table))

(defconst nlang-mode:indent-depth 4)

(defun nlang-mode:indentation (point offset)
  (cons point offset))

(defun nlang-mode:indentation:point (indentation)
  "Return the point of INDENTATION."
  (car indentation))

(defun nlang-mode:indentation:offset (indentation)
  "Return the offset of INDENTATION."
  (cdr indentation))

(defun nlang-mode:indent-line ()
  "Indent the current line."
  (let* ((indentation (save-excursion (nlang-mode:calculate-indentation)))
         (indentation-column
          (save-excursion
            (goto-char (nlang-mode:indentation:point indentation))
            (+ (current-column) (nlang-mode:indentation:offset indentation))))
         (current-indent
          (save-excursion (back-to-indentation) (current-column))))
    (if (<= (current-column) current-indent)
        ;; The cursor is on the left margin. Moving to the new indent.
        (indent-line-to indentation-column)
      ;; Keeps current relative position.
      (save-excursion (indent-line-to indentation-column)))))

(defun nlang-mode:tab-indent ()
  (let* ((indentation (save-excursion (nlang-mode:calculate-indentation)))
         (indentation-column
          (save-excursion
            (goto-char (nlang-mode:indentation:point indentation))
            (+ (current-column) (nlang-mode:indentation:offset indentation))))
         (current-indent
          (save-excursion (back-to-indentation) (current-column))))
    (if (>= current-indent indentation-column)
        ;; Already indented, indent 1 tab further
        (indent-line-to (+ indentation-column nlang-mode:indent-depth))
      (if (<= (current-column) current-indent)
          ;; The cursor is on the left margin. Moving to the new indent.
          (indent-line-to indentation-column)
        ;; Keeps current relative position.
        (save-excursion (indent-line-to indentation-column))))))

(defun nlang-mode:calculate-indentation ()
  "Calculate indent for the current line"
  (back-to-indentation)
  (let ((parser-state (syntax-ppss)))
    (cond
     ((nth 4 parser-state)
      ;; If the 4th element of `(syntax-ppss)' is non-nil, the cursor is on
      ;; the 2nd or following lines of a multiline comment, because:
      ;;
      ;; - The 4th element of `(syntax-ppss)' is nil on the comment starter.
      ;; - We have called `back-to-indentation`.
      (nlang-mode:calculate-indent-of-multiline-comment))

     ((eq (nth 3 parser-state) t)
      (nlang-mode:calculate-indent-of-multiline-string))

     ((looking-at "#")
      (nlang-mode:calculate-indent-of-single-line-comment))

     (t (nlang-mode:calculate-code-indent)))))

(defun nlang-mode:calculate-indent-of-multiline-comment ()
  (back-to-indentation)
  (let ((comment-beginning-position (nth 8 syntax-ppss))
        (starts-with-pipe (eq (char-after (+ (point) 1)) ?|)))
    (forward-line -1)
    (back-to-indentation)
    (cond
     ((<= (point) comment-beginning-position)
      ;; The cursor was on the 2nd line of the comment.
      (goto-char comment-beginning-position)
      (forward-char)
      ;; If there are extra characters or spaces after pipes, aligns with
      ;; the first non-space character or end of line.  Otherwise, aligns with
      ;; the first pipe.
      (when (and (looking-at "|*[^|\n]+") (not starts-with-pipe))
        (skip-chars-forward "|")
        (skip-syntax-forward " "))
      (nlang-mode:indentation (point) 0))

     ;; The cursor was on the 3rd or following lines of the comment.

     ((= (save-excursion
           (forward-line)
           (back-to-indentation)
           (point))
         (save-excursion
           (goto-char comment-beginning-position)
           (if (forward-comment 1)
               (progn
                 (backward-char)
                 (skip-chars-backward "|")
                 (point))
             -1)))
      ;; Before the closing delimiter.  Aligns with the first pipe of the
      ;; opening delimiter.
      (goto-char comment-beginning-position)
      (forward-char)
      (nlang-mode:indentation (point) 0))

     ;; Otherwise, aligns with a non-empty preceding line.

     ((and (bolp) (eolp))
      ;; The previous line is empty, so seeks a non-empty-line.
      (nlang-mode:calculate-indent-of-multiline-comment))

     (t
      ;; The previous line is not empty, so aligns to this line.
      (nlang-mode:indentation (point) 0)))))

(defun nlang-mode:calculate-code-indent ()
  "Return the indentation of the current line outside multiline comments."
  (back-to-indentation)
  (nlang-mode:indentation
   (point)
   (let ((net-braces (nth 0 (syntax-ppss)))
         (current-indent
          (- (point) (save-excursion (beginning-of-line) (point))))
         (end-brace-p
          (and (not (eolp))
               (seq-contains-p '(?} ?\] ?\)) (char-after)))))
     (save-excursion
       (forward-line -1)
       (back-to-indentation)
       (while (and (bolp) (eolp) (not (bobp)))
         ;; Ignore all blank lines (stop at beginning of file)
         (forward-line -1)
         (back-to-indentation))
       (while (nth 4 (syntax-ppss))
         ;; In a multiline comment, go back to comment start
         (forward-line -1)
         (back-to-indentation))
       (let* ((line-ends-with-open
               (save-excursion
                 (end-of-line)
                 (seq-contains-p '(?\( ?\[ ?{) (char-before))))
              (line-starts-with-close
               (seq-contains-p '(?\) ?\] ?}) (char-after)))
              (last-braces (nth 0 (syntax-ppss)))
              (last-indent
               (- (point) (save-excursion (beginning-of-line) (point))))
              (tab-delta
               (if line-starts-with-close
                   (if (and end-brace-p (not line-ends-with-open)) 0 -1)
                 (if (and end-brace-p (not line-ends-with-open)) 1 0))))
         (- (+ last-indent
               (* nlang-mode:indent-depth (- net-braces last-braces tab-delta)))
            current-indent))))))

(defun nlang-mode:calculate-indent-of-multiline-string ()
  "Return the indentation of the current line inside a multiline string."
  (back-to-indentation)
  (nlang-mode:indentation (point) 0))

(defun nlang-mode:calculate-indent-of-single-line-comment ()
  "Return the indentation of the current line inside a single-line comment."
  (cond
   ((save-excursion
      (forward-line 0)
      (bobp))
    (nlang-mode:indentation (point-min) 0))
   ((save-excursion
      (forward-line -1)
      (skip-syntax-forward " ")
      (looking-at "#"))
    (forward-line -1)
    (skip-syntax-forward " ")
    (nlang-mode:indentation (point) 0))
   (t
    (nlang-mode:calculate-code-indent))))

(defun nlang-mode:f-string-p (ppss)
  "Return non-null if PPSS is found inside an f-string"
  (require 'cl-lib)
  (and (nth 3 ppss)
       (cl-loop for spos = (1- (nth 8 ppss)) then (1- spos)
                while (and (<= (point-min) spos)
                           (memq (char-syntax (char-after spos)) '(?w ?_)))
                thereis (memq (char-after spos) '(?f ?F)))))

(defun nlang-mode:brace-is-escaped ()
  "Check if the brace at the previous point is escaped or not."
  (save-excursion
    (backward-char 1)
    (cl-assert (and (nlang-mode:f-string-p (syntax-ppss))
                  (memq (char-after) '(?\{ ?\}))))
    (let ((bs-count 0))
      (while (char-equal (char-before) ?\\)
        (backward-char 1)
        (cl-incf bs-count))
      (not (zerop (mod bs-count 2))))))

(defun nlang-mode:font-lock-f-strings (limit)
  "Mark the area in between braces in f-strings as being code.
Do this by removing the face property from the { ... } sections."
  (let ((ppss (syntax-ppss)))
    (while
        (progn
          (while (and (not (nlang-mode:f-string-p ppss))
                      (re-search-forward
                       (rx (any ?f ?F) (* word) (any ?\' ?\")) limit 'move))
            (setq ppss (syntax-ppss)))
          (< (point) limit))
      (cl-assert (nlang-mode:f-string-p ppss))
      (let ((send (save-excursion
                    (goto-char (nth 8 ppss))
                    (condition-case nil
                        (progn (let ((forward-sexp-function nil))
                                 (forward-sexp 1))
                               (min limit (1- (point))))
                      (scan-error limit)))))
        (while (search-forward "{" send t)
          (unless (nlang-mode:brace-is-escaped)
            (let ((beg (point))
                  (end (condition-case nil
                           (progn (up-list 1) (min send (point)))
                         (scan-error send))))
              (goto-char end)
              (put-text-property beg (1- end) 'face nil)
              (when (search-backward "!" beg t)
                (let ((bang-pos (point)))
                  (unless (re-search-forward (rx (any "[]{}()\"'")) (1- end) t)
                    (put-text-property bang-pos (1- end) 'face 'font-lock-keyword-face)))))))
        (goto-char (min limit (1+ send)))
        (setq ppss (syntax-ppss))))))

(defconst nlang-mode:file-defined (make-hash-table))

(defun nlang-mode:calculate-defined ()
  "Calculate the list of names defined in the current buffer"
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     until (eobp)
     with regex = (rx (or "class" "interface" "enum" "union"))
     for index = (string-match regex (thing-at-point 'line))
     if index
     collect
     (save-excursion
       (forward-char (+ index (length "class")))
       (forward-thing 'word)
       (thing-at-point 'word))
     do (forward-line 1))))

(defun nlang-mode:get-file-defined ()
  "Get the list of names defined in the current buffer"
  (let* ((name (buffer-name))
         (buffer (current-buffer))
         (buf-val (gethash name nlang-mode:file-defined (cons nil nil)))
         (buf-hash (car buf-val))
         (buf-vals (cdr buf-val))
         (current-hash (secure-hash 'md5 buffer)))
    (if (and buf-hash (= buf-hash current-hash))
        buf-vals
      (let ((defined (nlang-mode:calculate-defined)))
        (puthash name (cons buf-hash defined))
       defined))))

(define-derived-mode nlang-mode prog-mode "Nlang"
  "Major mode for the N language"
  (setq font-lock-defaults '((nlang-font-lock-keywords))))

(add-hook
 'nlang-mode-hook
 (lambda () (setq indent-line-function 'nlang-mode:indent-line)))

(add-hook 'nlang-mode-hook 'rainbow-delimiters-mode)

(add-to-list 'auto-mode-alist '("\\.newlang\\'" . nlang-mode))
