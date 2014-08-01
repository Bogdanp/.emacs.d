;; EMACS bindings
;; ~~~~~~~~~~~~~~
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c g") 'multi-occur-in-matching-buffers)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c p") 'prodigy)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-i") 'imenu)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)


;; Frame bindings
;; ~~~~~~~~~~~~~~
(global-set-key (kbd "C-c 1 M-c") 'create-frame-on-1st-monitor)
(global-set-key (kbd "C-c 2 M-c") 'create-frame-on-2nd-monitor)
(global-set-key (kbd "C-c 1 M-m") 'move-current-frame-to-1st-monitor)
(global-set-key (kbd "C-c 2 M-m") 'move-current-frame-to-2nd-monitor)
(global-set-key (kbd "C-c M-o") 'other-frame)
(global-set-key (kbd "C-c M-d") 'delete-frame)


;; MC bindings
;; ~~~~~~~~~~~
(global-set-key (kbd "C-c C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-.") 'mc/mark-next-like-this)

(define-key evil-normal-state-map (kbd "gcc") 'mc/mark-all-dwim)
(define-key evil-normal-state-map (kbd "gcr") 'mc/mark-all-like-this-dwim)
(define-key evil-normal-state-map (kbd "gcs") 'mc/mark-all-symbols-like-this)


;; Prodigy bindings
;; ~~~~~~~~~~~~~~~~
(define-key prodigy-view-mode-map (kbd "C-c f") 'python:trace-find-file-at-point)


;; Auto completion bindings
;; ~~~~~~~~~~~~~~~~~~~~~~~~
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)


;; Haskell-mode bindings
;; ~~~~~~~~~~~~~~~~~~~~~
(define-key haskell-mode-map (kbd "TAB") 'ac-complete)
(define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
(define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c v c") 'haskell-cabal-visit-file)


;; Term bindings
;; ~~~~~~~~~~~~~
(global-set-key (kbd "C-c M-a") 'term:toggle)

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-escape-map "c" 'term:add)
            (define-key term-raw-escape-map "\C-k" 'term:kill)
            (define-key term-raw-escape-map "\C-n" 'term:next)
            (define-key term-raw-escape-map "\C-p" 'term:prev)
            (define-key term-raw-escape-map "\C-y"
              (lambda ()
                (interactive)
                (term-send-raw-string (get-clipboard-value))))))

;; EVIL bindings
;; ~~~~~~~~~~~~~
;; Toggle column highlight.
(define-key evil-normal-state-map (kbd ",c") 'column-highlight-mode)

;; Misc
(define-key evil-normal-state-map (kbd ",,") 'evil-ex-nohighlight)

;; Ace Jump Mode
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "S-SPC") 'ace-jump-char-mode)

;; Window Management
(define-key evil-normal-state-map (kbd "C-w f") 'window:toggle-fullscreen)

;; Agenda Bookmarks
(define-key evil-normal-state-map (kbd ",a") 'org-agenda)

;; Jump to matching paren/bracket/object. This is basically an alias
;; for % which I find awkward to use.
(define-key evil-normal-state-map (kbd "TAB") 'evil-jump-item)

;; Bookmarks
(define-key evil-normal-state-map (kbd ",bc") 'bookmark-set)
(define-key evil-normal-state-map (kbd ",bl") 'list-bookmarks)
(define-key evil-normal-state-map (kbd ",bb") 'bookmark-jump)

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

;; Org-mode bindings for EVIL purposes.
(evil-define-key 'normal org-mode-map
  "gu" 'outline-up-heading
  "gk" 'org-backward-heading-same-level
  "gj" 'org-forward-heading-same-level)


(provide 'init-bindings)
