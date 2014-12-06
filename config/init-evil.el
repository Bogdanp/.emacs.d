;; EVIL
;; ~~~~
;; Turn evil-mode on.
(evil-mode t)


;; Config
;; ~~~~~~
;; Default to EMACS mode in these modes.
(dolist (mode '(calendar-mode
                comint-mode
                compilation-mode
                cider-repl-mode
                cider-stacktrace-mode
                debugger-mode
                diff-mode
                dired-mode
                erc-mode
                eshell-mode
                eww-mode
                eww-bookmark-mode
                eww-history-mode
                git-commit-mode
                grep-mode
                haskell-interactive-mode
                help-mode
                Info-mode
                special-mode
                paradox-commit-list-mode
                paradox-menu-mode
                prodigy-mode
                term-mode
                undo-tree-visualizer-mode))
  (evil-set-initial-state mode 'emacs))

;; Git-timemachine should default to EMACS mode as well.
(defun my-git-timemachine-mode-hook-for-evil ()
  (evil-emacs-state))

(add-hook 'git-timemachine-mode-hook 'my-git-timemachine-mode-hook-for-evil)


;; Fixes
;; ~~~~~
;; Make C-w work in the minibuffer.
(defun my-minibuffer-setup-hook-for-evil ()
  (local-set-key (kbd "C-w") 'backward-kill-word))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook-for-evil)

;; Fix clipboard dirtying.
(defun my-evil-local-mode-hook ()
  (setq-local interprogram-cut-function nil)
  (setq-local interprogram-paste-function nil))

(add-hook 'evil-local-mode-hook 'my-evil-local-mode-hook)

;; Fix copy-on-motion.
(defadvice evil-visual-update-x-selection (around clobber-x-select-text activate)
  (fset 'old-x-select-text (symbol-function 'x-select-text))
  (fmakunbound 'x-select-text)
  ad-do-it
  (fset 'x-select-text (symbol-function 'old-x-select-text)))


;; Extras
;; ~~~~~~
;; EVIL surround.
(require 'evil-surround)
(global-evil-surround-mode 1)


(provide 'init-evil)
