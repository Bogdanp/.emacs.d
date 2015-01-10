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
  :ensure t)


(provide 'init-company)
