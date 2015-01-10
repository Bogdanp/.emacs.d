;;; Global
(bind-keys ("C-j"     . newline-and-indent)
           ("C-w"     . backward-kill-word)
           ("C--"     . text-scale-decrease)
           ("C-="     . text-scale-increase)
           ("C-+"     . text-scale-increase)
           ("C-x C-i" . imenu)
           ("C-c M-a" . bp-term-toggle))


;;; "localleader"
(bind-keys :map evil-normal-state-map
           ;; Bookmarks
           (",bb" . bookmark-jump)
           (",bc" . bookmark-set)
           (",bl" . list-bookmarks)

           ;; Misc
           (",," . evil-ex-nohighlight)
           (",x" . calc))


;;; NORMAL mode
(bind-keys :map evil-normal-state-map
           ;; Movement
           ("C-a" . evil-beginning-of-line)
           ("C-e" . evil-end-of-line)
           ("C-p" . evil-previous-line)
           ("C-n" . evil-next-line)

           ;; Windows
           ("C-w f" . bp-window-toggle-fullscreen))


;;; INSERT mode
(bind-keys :map evil-insert-state-map
           ("C-a" . beginning-of-line)
           ("C-e" . end-of-line)
           ("C-p" . evil-previous-line)
           ("C-n" . evil-next-line))


;;; VISUAL mode
(bind-keys :map evil-visual-state-map
           ("C-a" . evil-beginning-of-line)
           ("C-e" . evil-end-of-line)
           ("C-p" . evil-previous-line)
           ("C-n" . evil-next-line))


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
