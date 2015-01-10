;; ace-jump-mode
;; ~~~~~~~~~~~~~
(use-package ace-jump-mode
  :commands (ace-jump-mode ace-jump-char-mode)
  :diminish ace-jump-mode
  :ensure t
  :init
  (bind-keys :map evil-normal-state-map
   ("SPC" . ace-jump-mode)
   ("S-SPC" . ace-jump-char-mode)))


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
