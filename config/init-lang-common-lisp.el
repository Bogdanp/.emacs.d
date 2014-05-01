(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'pretty-lambda-mode)
(add-hook 'lisp-mode-hook 'slime-mode)

(setq inferior-lisp-program "sbcl"
      lisp-indent-function 'common-lisp-indent-function)

;; Auto completion
;; ~~~~~~~~~~~~~~~
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))


(provide 'init-lang-common-lisp)
