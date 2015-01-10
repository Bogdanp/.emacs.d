;; magit
;; ~~~~~
(use-package magit
  :bind ("C-c m" . magit-status)
  :diminish magit-auto-revert-mode
  :ensure t)


;; git-gutter
;; ~~~~~~~~~~
(use-package git-gutter
  :commands global-git-gutter-mode
  :diminish git-gutter-mode
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-git-gutter-mode)
  :config
  (setq git-gutter:hide-gutter t))


;; git-timemachine
;; ~~~~~~~~~~~~~~~
(use-package git-timemachine
  :defer t
  :ensure t)


(provide 'init-git)
