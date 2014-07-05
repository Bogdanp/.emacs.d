;; Auto completion and error checking
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(add-hook 'tuareg-mode-hook 'merlin-mode)

;; Make sure we use ac
(setq merlin-use-auto-complete-mode t)

(add-to-list 'auto-mode-alist '("\\.eliom" . tuareg-mode))



(provide 'init-lang-ocaml)
