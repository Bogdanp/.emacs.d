;; C Language
;; ~~~~~~~~~~
(setq c-default-style "bsd"
      c-basic-offset 4)


;; Hooks
;; ~~~~~
;; Setup indentation.
(defun my-c-mode-hook ()
  (c-set-offset 'arglist-intro '+))

(add-hook 'c-mode-hook 'my-c-mode-hook)

;; Setup auto-completion.
(defun my-c-mode-common-hook ()
  (setq-local ac-sources (append '(ac-source-clang) ac-sources)))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


(provide 'init-lang-c)
