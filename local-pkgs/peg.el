(setq peg-mode-font-lock-keywords
      (let
          ((peg-operators '("<-" "/" "&" "!" "?" "*" "+" "."))
           (peg-def-regexp "[a-zA-Z_]\\(?:[a-zA-Z0-9_]\\)*[ \t\n]*\\(?:<-\\)"))
        `((,(regexp-opt peg-operators 'symbols) . font-lock-keyword-face)
          (,peg-def-regexp . (1 font-lock-variable-name-face)))))

(define-derived-mode peg-mode prog-mode "PEG"
  "Major mode for PEG definitions"
  (setq font-lock-defaults '((peg-mode-font-lock-keywords))))
