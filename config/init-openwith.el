(openwith-mode t)

;; Customize apps
;; ~~~~~~~~~~~~~~
(setq -openwith-filetypes
      (mapconcat 'identity '("mp3" "mp4" "mov"
                             "avi" "vmw" "jpg"
                             "png") "\\|"))

;; OS X's `open` is real handy.
(setq openwith-associations
      `((,(concat "\\.\\(" -openwith-filetypes "\\)\\'") "open" (file))))


(provide 'init-openwith)
