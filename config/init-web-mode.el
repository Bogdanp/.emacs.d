(add-hook 'web-mode-hook
          (lambda ()
            (remove-hook 'prog-mode-hook 'esk-pretty-lambdas)
            (remove-hook 'prog-mode-hook 'esk-add-watchwords)
            (remove-hook 'prog-mode-hook 'idle-highlight-mode)))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'"   . web-mode))

(setq web-mode-engines-alist
      '(("django"  . "\\.html\\'")))

(setq web-mode-style-padding 4)
(setq web-mode-script-padding 4)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-enable-current-element-highlight t)

(provide 'init-web-mode)
