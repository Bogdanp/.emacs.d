(add-hook 'web-mode-hook
          (lambda ()
            (remove-hook 'prog-mode-hook 'esk-pretty-lambdas t)
            (remove-hook 'prog-mode-hook 'esk-add-watchwords t)
            (remove-hook 'prog-mode-hook 'idle-highlight-mode t)))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'"   . web-mode))

(setq web-mode-engines-alist
      '(("razor"  . "\\.scala\\.html\\'")
        ("django" . "\\.html\\'")))

(setq web-mode-style-padding 4)
(setq web-mode-script-padding 4)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-enable-current-element-highlight t)

(provide 'init-web-mode)
