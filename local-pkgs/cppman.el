(require 'polymode)
(require 'cl-lib)

(defun cppman (sym)
  "Loads the interactive C++ manual page for SYM"
  (interactive "sName of symbol: ")
  (let ((executable (executable-find "cppman"))
        (bufname (concat "*Cppman " sym "*")))
    (with-current-buffer-window bufname nil nil
      (poly-cppman-mode)
      (call-process executable nil bufname nil sym)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward "\u2010" nil t)
          (replace-match "-" nil t))
        (goto-char (point-min))
        (while (re-search-forward cppman-reference-regex nil t)
          (make-button (match-beginning 0) (match-end 0)
                       :type 'cppman-button))))))

(defconst cppman-reference-regex
  (rx (any alpha "_:+*-") (+ (any alpha "_:+~!*<>()=-" ?\u2010))
      " " (? " ") "(" num (? alpha) ")"))

(defvar cppman-font-lock-keywords nil)

(setq cppman-font-lock-keywords
      (let ((reference cppman-reference-regex)
            (title (rx bol word (+ anychar) "(" (+ num) (** 1 2 alpha) ")" (* nonl)))
            (section (rx bol alpha (* (any alpha "_ :-" ?\u2010)) alpha eol))
            (subheading (rx bol (= 3 whitespace) alpha (* (any alpha ?\s)) alpha eol))
            (option-desc
             (rx bol (* whitespace) (any ?\+ ?\- ?\u2010) alnum (* (not whitespace))))
            (long-option-desc
             (rx bol (* whitespace) ?\- ?\- (any alnum ?\- ?\u2010) (* (not whitespace)))))
        `((,reference . font-lock-preprocessor-face)
          (,title . font-lock-keyword-face)
          (,section . font-lock-keyword-face)
          (,subheading . font-lock-function-name-face)
          (,option-desc . font-lock-constant-face)
          (,long-option-desc . font-lock-constant-face))))

(define-button-type 'cppman-button
  'action #'cppman:press-button
  'face 'font-lock-preprocessor-face)

(defun cppman:press-button (button)
  (let ((buffer-text (buffer-substring-no-properties
                      (button-start button) (button-end button))))
    (cppman (if (string-match (rx ?\( num (? alpha) ?\) eol) buffer-text)
                (string-trim (substring buffer-text 0 (match-beginning 0)))
              buffer-text))))

(define-derived-mode cppman-mode text-mode "Cppman"
  "Major mode for cppman pages"
  (setq font-lock-defaults '((cppman-font-lock-keywords))))

(define-hostmode cppman-hostmode :mode 'cppman-mode)

(define-innermode cppman-synopsis-innermode
  :mode 'c++-mode
  :head-matcher (rx bol "SYNOPSIS" (* whitespace) ?\n)
  :tail-matcher (rx bol (+ upper) (* whitespace) eol)
  :head-mode 'host
  :tail-mode 'host)

(define-innermode cppman-example-innermode
  :mode 'c++-mode
  :head-matcher (rx bol "EXAMPLE" (* whitespace) ?\n)
  :tail-matcher (rx bol (= 7 ?\s) (not ?\s) (* nonl))
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-cppman-mode
  :hostmode 'cppman-hostmode
  :innermodes '(cppman-synopsis-innermode cppman-example-innermode))

(add-to-list 'auto-mode-alist `(,(rx ?\* "Cppman " (* nonl) ?\*) . cppman-mode))
