;; OCaml Language
;; ~~~~~~~~~~~~~~
(add-to-list 'auto-mode-alist '("\\.eliom"     . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.topml$"    . tuareg-mode))


;; utop
;; ~~~~
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)


;; Merlin
;; ~~~~~~
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)


;; Hooks
;; ~~~~~
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'tuareg-mode-hook 'merlin-mode)


(provide 'init-lang-ocaml)
