;; Projectile
;; ~~~~~~~~~~
(use-package projectile
  :commands projectile-global-mode
  :ensure t
  :init
  (progn
    (add-hook 'after-init-hook #'projectile-global-mode))
  :config
  (progn
    ;; Enable caching.
    (setq projectile-enable-caching t)))


(provide 'init-projectile)
