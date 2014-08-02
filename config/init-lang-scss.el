;; Stupid functionality is stupid.
(setq scss-compile-at-save nil)

;; Indentation
;; ~~~~~~~~~~~
(add-hook 'scss-mode-hook (lambda ()
                            (setq-local css-indent-offset 2)))


(provide 'init-lang-scss)
