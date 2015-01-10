;;; EMACS bindings
(bind-keys ("C-j"     . newline-and-indent)
           ("C-w"     . backward-kill-word)
           ("C--"     . text-scale-decrease)
           ("C-="     . text-scale-increase)
           ("C-+"     . text-scale-increase)
           ("C-x C-i" . imenu)
           ("C-c M-a" . bp-term-toggle))

;; This is necessary for term-mode otherwise we run into the font issue.
(defun my-term-mode-bindings-hook ()
  (define-key term-raw-escape-map "c" 'bp-term-add)
  (define-key term-raw-escape-map "\C-k" 'bp-term-kill)
  (define-key term-raw-escape-map "\C-n" 'bp-term-next)
  (define-key term-raw-escape-map "\C-p" 'bp-term-prev)
  (define-key term-raw-escape-map "\C-r" 'rename-buffer)
  (define-key term-raw-escape-map "\C-y"
    (lambda ()
      (interactive)
      (term-send-raw-string (get-clipboard-value)))))

(add-hook 'term-mode-hook 'my-term-mode-bindings-hook)


;;; EVIL bindings
;; Misc
(bind-keys :map evil-normal-state-map
           ;; Jump to matching paren/bracket/object. This is basically
           ;; an alias for % which I find awkward to use.
           ("TAB" . evil-jump-item)

           ;; Bookmarks
           (",bb" . bookmark-jump)
           (",bc" . bookmark-set)
           (",bl" . list-bookmarks)

           ;; Misc
           (",," . evil-ex-nohighlight)
           (",x" . calc)

           ;; Windows
           ("C-w f" . bp-window-toggle-fullscreen))


;; Useful EMACS bindings in all modes.
(define-key evil-normal-state-map (kbd "C-a") 'evil-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-visual-state-map (kbd "C-a") 'evil-beginning-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "C-p") 'evil-previous-line)
(define-key evil-insert-state-map (kbd "C-p") 'evil-previous-line)
(define-key evil-visual-state-map (kbd "C-p") 'evil-previous-line)
(define-key evil-normal-state-map (kbd "C-n") 'evil-next-line)
(define-key evil-insert-state-map (kbd "C-n") 'evil-next-line)
(define-key evil-visual-state-map (kbd "C-n") 'evil-next-line)


(provide 'init-bindings)
