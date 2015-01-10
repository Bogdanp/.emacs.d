;; Flycheck
;; ~~~~~~~~
(use-package flycheck
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook #'flycheck-mode))
  :config
  (progn
    (setq-default flycheck-disabled-checkers '(emacs-lisp
                                               emacs-lisp-checkdoc
                                               haskell-ghc
                                               html-tidy))))

;; Flycheck Haskell
;; ~~~~~~~~~~~~~~~~
(use-package flycheck-haskell
  :defer t
  :ensure t
  :init
  (progn
    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))


;; Flycheck C and C++
;; ~~~~~~~~~~~~~~~~~~
(use-package flycheck-irony
  :defer t
  :ensure t
  :config
  (progn
    (defun my-flycheck-irony-setup-hook ()
      (add-to-list 'flycheck-checkers 'irony))

    (add-hook 'irony-mode-hook #'my-flycheck-irony-setup-hook)))


(provide 'init-flycheck)
