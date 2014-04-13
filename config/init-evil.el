(add-to-list 'load-path (expand-file-name "~/sandbox/evil/"))

(require 'evil)

;; Turn evil-mode on.
(evil-mode 1)

;; Make C-w work in the minibuffer.
(add-hook 'minibuffer-setup-hook
          (lambda () (local-set-key (kbd "C-w") 'backward-kill-word)))

;; Fix clipboard dirtying.
(add-hook 'evil-local-mode-hook
          (lambda ()
            (setq-local interprogram-cut-function nil)
            (setq-local interprogram-paste-function nil)))

;; Fix copy-on-motion.
(defadvice evil-visual-update-x-selection (around clobber-x-select-text activate)
  (fset 'old-x-select-text (symbol-function 'x-select-text))
  (fmakunbound 'x-select-text)
  ad-do-it
  (fset 'x-select-text (symbol-function 'old-x-select-text)))

(provide 'init-evil)
