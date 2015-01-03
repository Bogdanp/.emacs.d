;; Javascript Language
;; ~~~~~~~~~~~~~~~~~~~
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))


;; Hooks
;; ~~~~~
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source-inplace)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))

(defun my-web-mode-hook-for-flycheck ()
  (when (or (equal web-mode-content-type "javascript")
            (equal web-mode-content-type "jsx"))
    (flycheck-select-checker 'jsxhint-checker)
    (flycheck-mode 1)))

(add-hook 'web-mode-hook #'my-web-mode-hook-for-flycheck)


(provide 'init-lang-javascript)
