(setq c-default-style "bsd")
(setq c-basic-offset 4)

;; Setup indentation.
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-offset 'arglist-intro '+)))

;; Setup auto-completion.
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq ac-sources (append '(ac-source-clang) ac-sources))))

(provide 'init-lang-c)
