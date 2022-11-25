;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Patrick Norton"
      user-mail-address "patrick.147.norton@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(load-theme 'atom-one-dark t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(load! "local-pkgs/nlang-mode")
(load! "local-pkgs/cppman")

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

;; (when (window-system)
;;   (set-frame-font "Fira Code"))
;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;                (48 . ".\\(?:x[a-zA-Z]\\)")
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;;                )
;;              ))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; Turn this off for now
(add-hook! 'dafny-mode-hook (prettify-symbols-mode -1))

(add-hook! ('LaTeX-mode-hook 'markdown-mode-hook) #'auto-fill-mode)

(add-hook! cdlatex-mode (setq cdlatex-use-dollar-to-ensure-math t))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c++17")
            (setq flycheck-gcc-language-standard "c++17")))

(add-hook 'c-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c17")
            (setq flycheck-gcc-language-standard "c17")))

(add-hook 'rustic-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (rainbow-delimiters-mode)))

;; (add-hook 'TeX-mode-hook #'prettify-symbols-mode)

(defvar pre-reload-treemacs-visibility nil)

(add-hook! 'doom-before-reload-hook
  (setq pre-reload-treemacs-visibility (treemacs-current-visibility)))

;; Prevent reloads from making treemacs disappear
(add-hook! 'doom-after-reload-hook
  (when (and (eq pre-reload-treemacs-visibility 'visible)
             (not (eq (treemacs-current-visibility) 'visible)))
    (+treemacs/toggle)))

;; set left margin to show git-gutter again
;; This probably works in Emacs 28, but not in 27
(when (< emacs-major-version 28)
  (after! git-gutter-fringe (set-fringe-mode nil)))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)

;; Use clippy in rust
(after! flycheck (cl-pushnew 'rustic-clippy flycheck-checkers))
;; NOTE: This won't work until flycheck-rust-cargo-has-command-p is fixed
;; See https://github.com/flycheck/flycheck/issues/1916

(after! flycheck
  ;; Fixes aforementioned bug in flycheck
  (define-advice flycheck-rust-cargo-has-command-p
      (:override (command) fix-untrimmed-cargo-list)
    (let ((cargo (funcall flycheck-executable-find "cargo")))
      (cl-some (lambda (x) (string-prefix-p command x))
               (mapcar #'string-trim-left
                       (ignore-errors (process-lines cargo "--list")))))))

;; (after! flycheck
;;   ;; Fixes aforementioned bug in flycheck
;;   (defun flycheck-rust-cargo-has-command-p (command)
;;     (let ((cargo (funcall flycheck-executable-find "cargo")))
;;       (cl-some (lambda (x) (string-prefix-p command x))
;;                (mapcar #'string-trim-left
;;                        (ignore-errors (process-lines cargo "--list")))))))

(after! lsp-mode
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-clippy-preference "on")
  (setq lsp-rust-analyzer-import-merge-behaviour "last")
  (setq lsp-rust-analyzer-import-granularity "module")
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (setq lsp-rust-analyzer-experimental-proc-attr-macros t)
  (require 'dap-gdb-lldb))

(setq rustic-flycheck-clippy-params "--message-format=json")

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

;; (add-hook! 'rustic-mode-hook #'rainbow-delimiters-mode-enable)

(add-hook! 'LaTeX-mode-hook #'hl-todo-mode)
(add-hook! 'LaTeX-mode-hook #'prettify-symbols-mode)

;; Set command-x to cut instead of to M-x
(defun cut-region (beg end)
  "Copies the text to the kill buffer and deletes the selected region."
  (interactive "r")
  (copy-region-as-kill beg end)
  (delete-region beg end))

(map! "s-x" #'cut-region)

(after! git-gutter (setq git-gutter:update-interval 2))

(defun set-rel-line-nos ()
  (setq display-line-numbers (if visual-line-mode 'visual 'relative)))

(defun set-abs-line-nos ()
  (setq display-line-numbers 'absolute))

;; FIXME: flycheck-bidi-setup doesn't exist yet
;; (after! (flycheck flycheck-bidi) (flycheck-bidi-setup))

;; LaTeX macro-folding should only run when TeX-fold-mode is active
(define-advice +latex-fold-last-macro-a
    (:around (oldfun &rest rest) fix-mode-active)
  (when (bound-and-true-p TeX-fold-mode)
    (apply oldfun rest)))
