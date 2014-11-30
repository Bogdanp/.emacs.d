;; Undo
;; ~~~~
(global-undo-tree-mode)

(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diffs t)


;; Backups
;; ~~~~~~~
(setq local-temp-dir (locate-user-emacs-file "temp"))

(setq auto-save-file-name-transforms    `((".*"   ,local-temp-dir t))
      backup-directory-alist            `((".*" . ,local-temp-dir))
      undo-tree-history-directory-alist `((".*" . ,local-temp-dir))
      undo-tree-auto-save-history t)

(setq backup-by-copying t)


(provide 'init-backup)
