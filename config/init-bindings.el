;;; init-bindings.el --- binding configuration file
;;; Commentary:
;;; Code:
;;; Global
(bind-keys ("C-j"     . newline-and-indent)
           ("C-w"     . backward-kill-word)
           ("C--"     . text-scale-decrease)
           ("C-="     . text-scale-increase)
           ("C-+"     . text-scale-increase)
           ("C-c M-a" . bp-term-toggle))

(provide 'init-bindings)
;;; init-bindings.el ends here
