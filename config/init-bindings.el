;;; Global
(bind-keys ("C-j"     . newline-and-indent)
           ("C-w"     . backward-kill-word)
           ("C--"     . text-scale-decrease)
           ("C-="     . text-scale-increase)
           ("C-+"     . text-scale-increase)
           ("C-c M-a" . bp-term-toggle))


;;; Term
(defun my-term-mode-bindings-hook ()
  "This is necessary for term-mode otherwise we run into the font
issue. "
  (bind-keys :map term-raw-escape-map
             ("c"    . bp-term-add)
             ("\C-k" . bp-term-kill)
             ("\C-n" . bp-term-next)
             ("\C-p" . bp-term-prev)
             ("\C-y" . bp-term-clipboard-paste)))

(add-hook 'term-mode-hook 'my-term-mode-bindings-hook)


(provide 'init-bindings)
