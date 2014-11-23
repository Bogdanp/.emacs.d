;; Flycheck
;; ~~~~~~~~
(global-flycheck-mode)

;; Don't Flycheck these.
(setq-default flycheck-disabled-checkers '(emacs-lisp
                                           emacs-lisp-checkdoc
                                           haskell-ghc
                                           html-tidy))


(provide 'init-flycheck)
