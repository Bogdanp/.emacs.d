;; EMACS bindings
;; ~~~~~~~~~~~~~~
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c p") 'prodigy)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-i") 'imenu)
(global-set-key (kbd "C-c C-w u") 'winner-undo)
(global-set-key (kbd "C-c C-w r") 'winner-redo)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-increase)


;; Frame bindings
;; ~~~~~~~~~~~~~~
(global-set-key (kbd "C-c 1 M-c") 'create-frame-on-1st-monitor)
(global-set-key (kbd "C-c 2 M-c") 'create-frame-on-2nd-monitor)
(global-set-key (kbd "C-c 1 M-m") 'move-current-frame-to-1st-monitor)
(global-set-key (kbd "C-c 2 M-m") 'move-current-frame-to-2nd-monitor)
(global-set-key (kbd "C-c M-o") 'other-frame)
(global-set-key (kbd "C-c M-d") 'delete-frame)


;; Scala-mode bindings
;; ~~~~~~~~~~~~~~~~~~~
(define-key scala-mode-map (kbd "C-c C-.") 'ensime-edit-definition-other-window)
(define-key scala-mode-map (kbd "C-c .") 'ensime-edit-definition)
(define-key scala-mode-map (kbd "C-c ,") 'ensime-pop-find-definition-stack)


;; Python-mode bindings
;; ~~~~~~~~~~~~~~~~~~~~
(define-key python-mode-map (kbd "C-x C-e") 'bp-python-eval-region)

(evil-define-key 'normal python-mode-map
  ",r" 'py-test-run-test-at-point
  ",T" 'py-test-run-directory
  ",t" 'py-test-run-file)


;; Prodigy bindings
;; ~~~~~~~~~~~~~~~~
(define-key prodigy-view-mode-map (kbd "C-c f") 'bp-python-trace-find-file-at-point)


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
(global-set-key (kbd "C-c M-a") 'bp-term-toggle)

;; This is necessary for term-mode otherwise we run into the font issue.
(defun my-term-mode-bindings-hook ()
  (define-key term-raw-escape-map "c" 'bp-term-add)
  (define-key term-raw-escape-map "\C-k" 'bp-term-kill)
  (define-key term-raw-escape-map "\C-n" 'bp-term-next)
  (define-key term-raw-escape-map "\C-p" 'bp-term-prev)
  (define-key term-raw-escape-map "\C-y"
    (lambda ()
      (interactive)
      (term-send-raw-string (get-clipboard-value)))))

(add-hook 'term-mode-hook 'my-term-mode-bindings-hook)


;; EVIL bindings
;; ~~~~~~~~~~~~~
;; Misc
(define-key evil-normal-state-map (kbd ",,") 'evil-ex-nohighlight)
(define-key evil-normal-state-map (kbd ",x") 'calc)

;; Ace Jump Mode
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "S-SPC") 'ace-jump-char-mode)

;; Window Management
(define-key evil-normal-state-map (kbd "C-w f") 'bp-window-toggle-fullscreen)

;; Org-capture
(define-key evil-normal-state-map (kbd ",c") 'org-capture)

;; Agenda Bookmarks
(define-key evil-normal-state-map (kbd ",a") 'org-agenda)

;; Jump to matching paren/bracket/object. This is basically an alias
;; for % which I find awkward to use.
(define-key evil-normal-state-map (kbd "TAB") 'evil-jump-item)

;; Bookmarks
(define-key evil-normal-state-map (kbd ",bb") 'bookmark-jump)
(define-key evil-normal-state-map (kbd ",bc") 'bookmark-set)
(define-key evil-normal-state-map (kbd ",bl") 'list-bookmarks)

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
  ;; Tasks
  ",ta" 'bp-org-archive-task-at-point

  ;; Movement
  "gu" 'outline-up-heading
  "gk" 'org-backward-heading-same-level
  "gj" 'org-forward-heading-same-level
  "gp" 'org-backward-element
  "gn" 'org-forward-element)


(provide 'init-bindings)
