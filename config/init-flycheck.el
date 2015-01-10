;; Flycheck
;; ~~~~~~~~
(use-package flycheck
  :commands (flycheck-mode flycheck-define-checker)
  :defer t
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook #'flycheck-mode))

    (setq-default flycheck-disabled-checkers '(emacs-lisp
                                               emacs-lisp-checkdoc
                                               haskell-ghc
                                               html-tidy)))

;; Flycheck Haskell
;; ~~~~~~~~~~~~~~~~
(use-package flycheck-haskell
  :defer t
  :ensure t
  :config
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


;; Custom checkers
;; ~~~~~~~~~~~~~~~
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source-inplace)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))


(provide 'init-flycheck)
