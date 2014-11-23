;; Stupid functionality is stupid.
(setq scss-compile-at-save nil)

;; Indentation
;; ~~~~~~~~~~~
(defun my-scss-mode-hook ()
  (setq-local css-indent-offset 2))

(add-hook 'scss-mode-hook 'my-scss-mode-hook)


(provide 'init-lang-scss)
