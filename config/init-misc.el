;; bind-key
;; ~~~~~~~~
;; This is required by use-package.
(use-package bind-key
  :commands (bind-key bind-key*)
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
  :commands restclient-mode
  :ensure t)


;; paradox
;; ~~~~~~~
;; A better package management UI.
(use-package paradox
  :commands paradox-list-packages
  :ensure t
  :config
  (progn
    (setq paradox-github-token t)))


(provide 'init-misc)
