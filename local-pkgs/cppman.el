;;; cppman.el --- Integration of cppman

;; Copyright (C) 2023 Patrick Norton


;; Author: Patrick Norton
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: languages, tools
;; URL: https://github.com/PatrickNorton/.doom.d

;;; Commentary:

;; This package provides a command to call `cppman', a command-line utility for
;; querying C and C++ documentation. It also provides a minor mode that allows
;; for nicer viewing of said documents.

;;; Code:

(require 'polymode)
(require 'cl-lib)

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
  'action #'cppman--press-button
  'face 'font-lock-preprocessor-face)

(defun cppman--press-button (button)
  "Open the text of the button BUTTON in a new `cppman' query."
  (let ((buffer-text (buffer-substring-no-properties
                      (button-start button) (button-end button))))
    (cppman (if (string-match (rx ?\( num (? alpha) ?\) eol) buffer-text)
                (string-trim (substring buffer-text 0 (match-beginning 0)))
              buffer-text))))

;;;###autoload
(define-derived-mode cppman-mode special-mode "Cppman"
  "Major mode for cppman pages."
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

;;;###autoload (autoload 'poly-cppman-mode "cppman")
(define-polymode poly-cppman-mode
  :hostmode 'cppman-hostmode
  :innermodes '(cppman-synopsis-innermode cppman-example-innermode))

;;;###autoload
(defun cppman (sym)
  "Load the interactive C++ manual page for SYM."
  (interactive "sName of symbol: ")
  (let ((executable (executable-find "cppman"))
        (bufname (concat "*Cppman " sym "*")))
    (when (null executable)
      (error "Could not find `cppman' executable"))
    (with-current-buffer-window bufname nil nil
      (poly-cppman-mode)
      (unwind-protect
          (progn
            (setf inhibit-read-only t)
            (erase-buffer)
            (call-process executable nil bufname nil sym)
            ;; Replace Unicode dashes with standard hyphens
            (save-excursion
              (goto-char (point-min))
              (while (search-forward "\u2010" nil t)
                (replace-match "-" nil t))
              ;; Add clickable buttons where relevant
              (goto-char (point-min))
              (while (re-search-forward cppman-reference-regex nil t)
                (make-button (match-beginning 0) (match-end 0)
                             :type 'cppman-button))))
        (setf inhibit-read-only nil)))))

;;;###autoload
(add-to-list 'auto-mode-alist `(,(rx ?\* "Cppman " (* nonl) ?\*) . cppman-mode))

(provide 'cppman)

;;; cppman.el ends here
