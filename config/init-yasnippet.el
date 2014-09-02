;; Only use my snippets.
(setq yas-snippet-dirs `(,(expand-file-name "~/.emacs.d/snippets")))

;; Preload all snippets.
(yas-reload-all)


(provide 'init-yasnippet)
