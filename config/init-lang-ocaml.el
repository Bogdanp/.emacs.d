;; Auto completion and error checking
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

;; Make sure we use ac
(setq merlin-use-auto-complete-mode t)


(provide 'init-lang-ocaml)
