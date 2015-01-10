;; Projectile
;; ~~~~~~~~~~
(use-package projectile
  :commands projectile-global-mode
  :ensure t
  :init
  (add-hook 'after-init-hook #'projectile-global-mode)
  :config
  (setq projectile-enable-caching t))


(provide 'init-projectile)
