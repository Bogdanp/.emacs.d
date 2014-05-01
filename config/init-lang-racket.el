(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'pretty-lambda-mode)

(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))


(provide 'init-lang-racket)
