;; Set up extempore-mode
;; ~~~~~~~~~~~~~~~~~~~~~
(autoload 'extempore-mode (expand-file-name "~/sandbox/extempore/extras/extempore.el") "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))


(provide 'init-lang-extempore)
