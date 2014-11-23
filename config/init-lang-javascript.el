(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(custom-set-variables
 '(js2-mode-show-parse-errors nil)
 '(js2-highlight-external-variables nil)
 '(js2-mode-show-strict-warnings nil))


(defun my-js2-mode-hook ()
  (setq-local indent-tabs-mode nil))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)


(provide 'init-lang-javascript)
