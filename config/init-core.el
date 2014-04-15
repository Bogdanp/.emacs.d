;; Paths
;; ~~~~~
(setq default-directory "~/")
(setq exec-path
      (append exec-path
              `("/usr/local/bin"
                ,(expand-file-name "~/.cabal/bin"))))


;; UI
;; ~~
(set-cursor-color "#FFFFFF")
(set-default-font "Inconsolata-15")

(load-theme 'twilight-anti-bright t)

(when (window-system)
  (set-frame-position (selected-frame) 205 32)
  (set-frame-size (selected-frame) 185 65))


;; Editing
;; ~~~~~~~
;; Highlight current line.
(global-hl-line-mode t)

;; Don't wrap long lines.
(setq-default truncate-lines t)

(global-rainbow-delimiters-mode t)


(provide 'init-core)
