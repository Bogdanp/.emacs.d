(add-hook 'term-mode-hook (lambda ()
                            (evil-emacs-state)
                            (yas-minor-mode -1)))


(provide 'init-term)
