;; Initialize flycheck after loading the EMACS session.
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Disable flycheck on elisp.
(eval-after-load 'flycheck
  '(setq-default flycheck-disabled-checkers '(emacs-lisp
                                              emacs-lisp-checkdoc

                                              haskell-ghc

                                              html-tidy)))


(provide 'init-flycheck)
