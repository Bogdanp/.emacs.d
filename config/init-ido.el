;; ido-mode
;; ~~~~~~~~
;; ido-mode comes with EMACS.
(ido-mode +1)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-ignore-extensions t)


;; ido-ubiquitous
;; ~~~~~~~~~~~~~~
;; Use ido-mode everywhere.
(use-package ido-ubiquitous
  :ensure t
  :init
  (ido-ubiquitous-mode +1))


;; ido-vertical-mode
;; ~~~~~~~~~~~~~~~~~
(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode +1))


;; flx-ido
;; ~~~~~~~
;; Used by projectile.
(use-package flx-ido
  :ensure t)


;; Smex
;; ~~~~
(use-package smex
  :bind (("M-x" . smex)
         ("C-;" . smex))
  :ensure t
  :init
  (progn
    (smex-initialize))
  :config
  (progn
    (setq smex-save-file (concat user-emacs-directory ".smex-items"))))


(provide 'init-ido)
