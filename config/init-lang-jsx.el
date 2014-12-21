;; JSX
;; ~~~
;; Add syntax checking.
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."
  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (jsx-mode))

(defun my-jsx-mode-hook ()
  (flycheck-select-checker 'jsxhint-checker))

(add-hook 'jsx-mode-hook #'my-jsx-mode-hook)


(provide 'init-lang-jsx)
