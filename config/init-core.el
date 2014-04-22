;; Paths
;; ~~~~~
(setq default-directory "~/")
(setq exec-path
      (append exec-path
              `("/usr/local/bin"
                ,(expand-file-name "~/.cabal/bin"))))


;; UI
;; ~~
;; No bell of any kind.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()))

(set-cursor-color "#FFFFFF")
(set-default-font "Inconsolata-15")

(load-theme 'twilight-anti-bright t)

(when (window-system)
  (set-frame-position (selected-frame) 13 32)
  (set-frame-size (selected-frame) 234 65))


;; Editing
;; ~~~~~~~
;; Highlight current line.
(global-hl-line-mode t)

;; Don't wrap long lines.
(setq-default truncate-lines t)

(global-rainbow-delimiters-mode t)


(provide 'init-core)
