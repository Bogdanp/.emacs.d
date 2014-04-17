;; EMACS bindings
;; ~~~~~~~~~~~~~~
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c g") 'multi-occur-in-matching-buffers)
(global-set-key (kbd "C-c p") 'prodigy)
(global-set-key (kbd "<f5>") 'visit-ansi-term)


;; Prodigy bindings
;; ~~~~~~~~~~~~~~~~
(define-key prodigy-view-mode-map (kbd "C-c f") 'python:trace-find-file-at-point)


;; Auto completion bindings
;; ~~~~~~~~~~~~~~~~~~~~~~~~
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)


;; EVIL bindings
;; ~~~~~~~~~~~~~
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "S-SPC") 'ace-jump-char-mode)

;; Useful emacs bindings in all modes.
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
