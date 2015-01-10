;; Yasnippet
;; ~~~~~~~~~
(use-package yasnippet
  :diminish yas-minor-mode
  :defer t
  :ensure t
  :idle
  (progn
    (yas-reload-all)))


(provide 'init-yasnippet)
