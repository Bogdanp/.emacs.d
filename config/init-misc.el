;; bind-key
;; ~~~~~~~~
;; This is required by use-package.
(use-package bind-key
  :defer t
  :ensure t)


;; f
;; ~
;; Modern filesystem functions for EMACS.
(use-package f
  :defer t
  :ensure t)


;; fuzzy
;; ~~~~~
;; Fuzzy matching. Used by auto-complete.
(use-package fuzzy
  :defer t
  :ensure t)


;; restclient
;; ~~~~~~~~~~
(use-package restclient
  :defer t
  :ensure t)


;; paradox
;; ~~~~~~~
;; A better package management UI.
(use-package paradox
  :defer t
  :ensure t
  :config
  (progn
    (setq paradox-github-token t)))


(provide 'init-misc)
