;; Paths
;; ~~~~~
(setq default-directory "~/")
(setq exec-path
      (append exec-path
              `("/usr/local/bin"
                ,(expand-file-name "~/.cabal/bin"))))

(setq save-place t)


;; UI
;; ~~
;; Use y and n instead of yes and no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; No bell of any kind.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()))

(set-cursor-color "#FFFFFF")
(set-default-font "Inconsolata-15")

(load-theme 'twilight-anti-bright t)

(global-pretty-lambda-mode t)


;; Editing
;; ~~~~~~~
;; Highlight current line.
(global-hl-line-mode t)

;; Don't wrap long lines.
(setq-default truncate-lines t)

;; Highlight matching parens.
(show-paren-mode 1)

;; Show line and column number in status line.
(column-number-mode t)

(global-rainbow-delimiters-mode t)


;; Files
;; ~~~~~
;; Save a list of files that were visited recently.
(recentf-mode t)


(provide 'init-core)
