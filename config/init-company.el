;; Company
;; ~~~~~~~
(use-package company
  :commands company-mode
  :diminish company
  :ensure t
  :config
  (setq company-idle-delay 0.25))


;; C and C++ completion
;; ~~~~~~~~~~~~~~~~~~~~
(use-package company-irony
  :commands (company-irony-setup-begin-commands)
  :ensure t
  :init
  (progn
    (defun my-company-irony-setup-hook ()
      (add-to-list 'company-backends 'company-irony))

    (add-hook 'irony-mode-hook #'my-company-irony-setup-hook)
    (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)))


(provide 'init-company)
