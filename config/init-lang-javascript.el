(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook (lambda ()
                           (setq-local indent-tabs-mode nil)))


(provide 'init-lang-javascript)
