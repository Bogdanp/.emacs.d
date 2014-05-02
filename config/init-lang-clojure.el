(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'pretty-lambda-mode)

;; Auto completion
;; ~~~~~~~~~~~~~~~
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-compliment-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-compliment-repl-setup)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes cider-mode))


(provide 'init-lang-clojure)
