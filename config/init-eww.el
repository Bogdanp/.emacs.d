(defun my-eww-mode-hook ()
  (setq-local truncate-lines nil))

(add-hook 'eww-mode-hook #'my-eww-mode-hook)


(provide 'init-eww)
