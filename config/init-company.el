;; Company
;; ~~~~~~~
(use-package company
  :diminish company
  :ensure t
  :config
  (progn
    (setq company-idle-delay 0.25)))


;; C and C++ completion
;; ~~~~~~~~~~~~~~~~~~~~
(use-package company-irony
  :defer t
  :ensure t
  :config
  (progn
    (defun my-company-irony-setup-hook ()
      (add-to-list 'company-backends 'company-irony))

    (add-hook 'irony-mode-hook #'my-company-irony-setup-hook)
    (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)))


(provide 'init-company)
