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
  ;; Disable AC since its irony mode isn't ready yet.
  (auto-complete-mode -1)

  (eldoc-mode +1)
  (irony-eldoc +1)
  (company-mode +1)
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'flycheck-checkers 'irony))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook 'irony-mode)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)


(provide 'init-lang-c)
