;; C Language
;; ~~~~~~~~~~
(setq c-default-style "bsd"
      c-basic-offset 4)


;; irony-mode
;; ~~~~~~~~~~
(use-package irony
  :commands irony-mode
  :ensure t
  :init
  (add-hook 'c-mode-common-hook #'irony-mode)
  :config
  (progn
    (defun my-irony-mode-hook ()
      ;; Disable AC since its irony mode isn't ready yet.
      (auto-complete-mode -1)

      (eldoc-mode +1)
      (irony-eldoc +1)
      (company-mode +1))

    (add-hook 'irony-mode-hook #'my-irony-mode-hook)))

(use-package irony-eldoc
  :commands irony-eldoc
  :ensure t)


;; Hooks
;; ~~~~~
;; Setup indentation.
(defun my-c-mode-hook ()
  (c-set-offset 'arglist-intro '+))

(add-hook 'c-mode-hook 'my-c-mode-hook)


(provide 'init-lang-c)
