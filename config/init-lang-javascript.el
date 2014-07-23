(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(custom-set-variables
 '(js2-mode-show-parse-errors nil)
 '(js2-highlight-external-variables nil)
 '(js2-mode-show-strict-warnings nil))

(add-hook 'js2-mode-hook (lambda ()
                           (setq-local indent-tabs-mode nil)))


(provide 'init-lang-javascript)
