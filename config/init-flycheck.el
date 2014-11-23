;; Flycheck allthethings.
(global-flycheck-mode)

;; Disable flycheck on these.
(setq-default flycheck-disabled-checkers '(emacs-lisp
                                           emacs-lisp-checkdoc

                                           haskell-ghc

                                           html-tidy))


(provide 'init-flycheck)
