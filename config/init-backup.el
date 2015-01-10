;; Undo
;; ~~~~
(use-package undo-tree
  :commands global-undo-tree-mode
  :diminish undo-tree-mode
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-undo-tree-mode)
  :config
  (progn
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diffs t)))


;; Backups
;; ~~~~~~~
(setq local-temp-dir (expand-file-name (locate-user-emacs-file "temp")))

(setq auto-save-file-name-transforms    `((".*"   ,local-temp-dir t))
      backup-directory-alist            `((".*" . ,local-temp-dir))
      undo-tree-history-directory-alist `((".*" . ,local-temp-dir))
      undo-tree-auto-save-history t)

(setq backup-by-copying t)


(provide 'init-backup)
