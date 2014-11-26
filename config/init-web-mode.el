;; web-mode
;; ~~~~~~~~
(setq web-mode-code-indent-offset 4
      web-mode-style-indent-offset 4
      web-mode-script-indent-offset 4
      web-mode-markup-indent-offset 4

      web-mode-style-padding 4
      web-mode-script-padding 4

      web-mode-enable-auto-closing t
      web-mode-enable-auto-expanding t
      web-mode-enable-auto-pairing t
      web-mode-enable-current-element-highlight t)

;; Setup file extensions.
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'"   . web-mode))

;; Setup engines.
(setq web-mode-engines-alist
      '(("razor"  . "\\.scala\\.html\\'")
        ("django" . "\\.html\\'")))


;; Hooks
;; ~~~~~
(defun my-web-mode-hook ()
  ;; These things break web-mode so we need to disable them.
  (remove-hook 'prog-mode-hook 'esk-pretty-lambdas t)
  (remove-hook 'prog-mode-hook 'esk-add-watchwords t)
  (remove-hook 'prog-mode-hook 'idle-highlight-mode t))

(add-hook 'web-mode-hook 'my-web-mode-hook)


(provide 'init-web-mode)
