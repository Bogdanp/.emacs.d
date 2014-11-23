;; Hooks
;; ~~~~~
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'pretty-lambda-mode)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'pretty-lambda-mode)
(add-to-list 'ac-modes 'cider-mode)


(provide 'init-lang-clojure)
