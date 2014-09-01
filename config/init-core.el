;; Paths
;; ~~~~~
(setq default-directory "~/")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq exec-path
      (append exec-path
              `("/usr/local/bin"
                ,(expand-file-name "~/.cabal/bin")
                ,(expand-file-name "~/.opam/system/bin"))))

;; Save point position in each buffer.
(setq-default save-place t)

;; /path/to/buffer instead of buffer<n>
(setq uniquify-buffer-name-style 'forward)


;; UI
;; ~~
;; Use y and n instead of yes and no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; No bell of any kind.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()))

;; Disable tooltips.
(tooltip-mode -1)

;; Prevent the cursor from blinking.
(blink-cursor-mode -1)

;; White cursor is best cursor.
(set-cursor-color "#FFFFFF")

;; Pretty colors!!
(load-theme 'twilight-anti-bright t)


;; Editing
;; ~~~~~~~
;; Never use tabs.
(setq-default indent-tabs-mode nil)

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

;; Delete trailing whitespaces whenever a file gets saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; A more useful default grep command.
(grep-apply-setting 'grep-command "grep -irnHI -e ")


;; Windows
;; ~~~~~~~
(defvar window:previous-window-configuration nil
  "Holds the previous window configuration.")

(defun window:toggle-fullscreen ()
  "Toggle between whether or not the current window should be
maximized."
  (interactive)
  (if window:previous-window-configuration
      (progn
	(set-window-configuration window:previous-window-configuration)
	(setq window:previous-window-configuration nil))
    (progn
      (setq window:previous-window-configuration (current-window-configuration))
      (delete-other-windows))))


(provide 'init-core)
