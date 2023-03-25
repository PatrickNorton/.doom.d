;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; WARNING: This file is generated by editing config.org: changes here will be
;; overwritten.

(setq user-full-name "Patrick Norton"
      user-mail-address "patrick.147.norton@gmail.com")

(setq doom-font (font-spec :family "Fira Code Retina" :size (if IS-MAC 12 9.0)))
(setq doom-unicode-font doom-font)

(load-theme 'atom-one-dark t)

(setq org-directory "~/org/")

(setq display-line-numbers-type t)

(load! "local-pkgs/mips")

(add-load-path! "local-pkgs")
(use-package! nlang-mode)
(use-package! cppman)

(defun delete-word ()
  (print "In delete-word")
  (let ((bound (bounds-of-thing-at-point 'word)))
    (if bound
        (delete-region (car bound) (cdr bound))
      (error "No word at point"))))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(global-set-key (kbd "s-<backspace>") #'evil-delete-whole-line)

(global-set-key (kbd "s-<right>")  #'move-end-of-line)

(global-set-key (kbd "s-<left>") #'move-beginning-of-line)

(global-set-key (kbd "M-<backspace>") #'delete-word)

(global-set-key (kbd "M-<right>") #'forward-word)

(global-set-key (kbd "M-<left>") #'backward-word)

(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)

(defun cut-region (beg end)
  "Copies the text to the kill buffer and deletes the selected region."
  (interactive "r")
  (copy-region-as-kill beg end)
  (delete-region beg end))

(map! "s-x" #'cut-region)

(add-hook! 'dafny-mode-hook (prettify-symbols-mode -1))

(add-hook! ('LaTeX-mode-hook 'markdown-mode-hook 'org-mode-hook) #'auto-fill-mode)

(add-hook! cdlatex-mode (setq cdlatex-use-dollar-to-ensure-math t))

(add-hook! 'LaTeX-mode-hook #'hl-todo-mode)
(add-hook! 'LaTeX-mode-hook #'prettify-symbols-mode)

(define-advice +latex-fold-last-macro-a
    (:around (oldfun &rest rest) fix-mode-active)
  (when (bound-and-true-p TeX-fold-mode)
    (apply oldfun rest)))

(after! tex-mode
  (add-to-list 'tex--prettify-symbols-alist
               '("\\mathbb{C}" . ?ℂ)))

(after! cdlatex
  (setq! cdlatex-math-symbol-alist
         '((?R . ("\\mathbb{R}" "\\Re"))
           (?C . ("\\mathbb{C}" "" "\\arccos"))
           (?Z . ("\\mathbb{Z}" "" ""))))
  (cdlatex-compute-tables))

(defvar cdlatex-which-shortcut--most-recent nil)

(defun cdlatex-which-shortcut (symbol)
  (interactive
   (list (read-string
          (if cdlatex-which-shortcut--most-recent
              (format "Symbol (default %s): "
                      cdlatex-which-shortcut--most-recent)
            "Symbol: "))))
  (let* ((symb (if (string-empty-p symbol)
                   cdlatex-which-shortcut--most-recent
                 symbol))
         (proper-symbol (if (= (aref symb 0) ?\\)
                            symb
                          (concat "\\" symb))))
    (setf cdlatex-which-shortcut--most-recent proper-symbol)
    (message (or (cl-some (lambda (val)
                            (let ((index (seq-position (cdr val) proper-symbol)))
                              (if index
                                  (format "%c (level %d)" (car val) (1+ index))
                                nil)))
                          cdlatex-math-symbol-alist-comb)
                 "No sequence found"))))

(add-hook 'rustic-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (rainbow-delimiters-mode)))

(after! flycheck (cl-pushnew 'rustic-clippy flycheck-checkers))

(after! flycheck
  (define-advice flycheck-rust-cargo-has-command-p
      (:override (command) fix-untrimmed-cargo-list)
    (let ((cargo (funcall flycheck-executable-find "cargo")))
      (cl-some (lambda (x) (string-prefix-p command x))
               (mapcar #'string-trim-left
                       (ignore-errors (process-lines cargo "--list")))))))

(after! lsp-mode
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-clippy-preference "on")
  (setq lsp-rust-analyzer-import-granularity "module")
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (setq lsp-rust-analyzer-experimental-proc-attr-macros t)
  (require 'dap-gdb-lldb))

(after! dap-mode
  (setq dap-default-terminal-kind "integrated")
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil))
  (dap-auto-configure-mode +1))

(defvar pre-reload-treemacs-visibility nil)

(add-hook! 'doom-before-reload-hook
  (setq pre-reload-treemacs-visibility (and (fboundp 'treemacs-current-visibility)
                                            (treemacs-current-visibility))))

(add-hook! 'doom-after-reload-hook
  (when (and (eq pre-reload-treemacs-visibility 'visible)
             (not (eq (treemacs-current-visibility) 'visible)))
    (+treemacs/toggle)))

(add-hook! 'haskell-literate-mode-hook
  (visual-line-mode t))

(after! haskell-mode
  (setq! haskell-hoogle-command "hoogle"))

(add-hook! 'python-mode-hook
  (display-fill-column-indicator-mode 80))

(after! git-gutter (setq git-gutter:update-interval 2))

(when (< emacs-major-version 28)
  (after! git-gutter-fringe (set-fringe-mode nil)))

(add-hook! 'elfeed-search-mode-hook #'elfeed-update)

(after! vertico
  (setq! vertico-sort-function #'vertico-sort-length-alpha))
