;; Markdown
;; ~~~~~~~~
(use-package markdown-mode
  :commands markdown-mode
  :defer t
  :ensure t)


;; SCSS
;; ~~~~
(use-package scss-mode
  :commands scss-mode
  :defer t
  :ensure t
  :config
  (progn
    ;; Stupid functionality is stupid.
    (setq scss-compile-at-save nil)

    (defun my-scss-mode-hook ()
      (setq-local css-indent-offset 2))

    (add-hook 'scss-mode-hook 'my-scss-mode-hook)))


;; web-mode
;; ~~~~~~~~
(use-package web-mode
  :commands web-mode
  :defer t
  :ensure t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.php\\'"   . web-mode))
    (add-to-list 'auto-mode-alist '("\\.hbs\\'"   . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'"    . web-mode)))
  :config
  (progn
    (setq web-mode-code-indent-offset 4
          web-mode-style-indent-offset 4
          web-mode-script-indent-offset 4
          web-mode-markup-indent-offset 4

          web-mode-style-padding 4
          web-mode-script-padding 4

          web-mode-enable-auto-closing t
          web-mode-enable-auto-expanding t
          web-mode-enable-auto-pairing t
          web-mode-enable-current-element-highlight t

          web-mode-engines-alist '(("razor"  . "\\.scala\\.html\\'")
                                   ("django" . "\\.html\\'")))

    (defun my-web-mode-hook ()
      (setq-local ac-auto-start nil)

      ;; These things break web-mode so we need to disable them.
      (remove-hook 'prog-mode-hook 'esk-pretty-lambdas t)
      (remove-hook 'prog-mode-hook 'esk-add-watchwords t)
      (remove-hook 'prog-mode-hook 'idle-highlight-mode t))

    (defun my-web-mode-hook-for-flycheck ()
      (when (or (equal web-mode-content-type "javascript")
                (equal web-mode-content-type "jsx"))
        (flycheck-select-checker 'jsxhint-checker)
        (flycheck-mode 1)))

    (add-hook 'web-mode-hook #'my-web-mode-hook)
    (add-hook 'web-mode-hook #'my-web-mode-hook-for-flycheck)))


;; YAML
;; ~~~~
(use-package yaml-mode
  :commands yaml-mode
  :defer t
  :ensure t)


(provide 'init-lang-web)
