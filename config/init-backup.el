;; Undo
;; ~~~~
(global-undo-tree-mode)

;; Backups
;; ~~~~~~~
(setq auto-save-file-name-transforms    `((".*"   ,temporary-file-directory t))
      backup-directory-alist            `((".*" . ,temporary-file-directory))
      undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))
      undo-tree-auto-save-history t)

(setq backup-by-copying t)

(provide 'init-backup)
