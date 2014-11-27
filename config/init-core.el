;; Paths
;; ~~~~~
;; Home sweet home.
(setq default-directory "~/")

;; Read PATH from zsh on OS X.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Save point position in each buffer.
(setq-default save-place t)

;; /path/to/buffer instead of buffer<n>.
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

;; Fuck electric-indent-mode.
(electric-indent-mode -1)


;; Files
;; ~~~~~
;; Save a list of files that were visited recently.
(recentf-mode t)

;; Delete trailing whitespaces whenever a file gets saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; A more useful default grep command.
(grep-apply-setting 'grep-command "grep -irnHI -e ")

;; Make default dired slightly nicer.
(setq insert-directory-program "/usr/local/bin/gls")
(setq dired-listing-switches "--group-directories-first -alh")

;; Revert files that update on disk automatically. Ignores dirty buffers.
(global-auto-revert-mode 1)


;; Windows
;; ~~~~~~~
(defvar bp-window-previous-window-configuration nil
  "Holds the previous window configuration.")

(defun bp-window-toggle-fullscreen ()
  "Toggle between whether or not the current window should be
maximized."
  (interactive)
  (if bp-window-previous-window-configuration
      (progn
	(set-window-configuration bp-window-previous-window-configuration)
	(setq bp-window-previous-window-configuration nil))
    (progn
      (setq bp-window-previous-window-configuration (current-window-configuration))
      (delete-other-windows))))

;; winner-mode
(winner-mode 1)


;; Regexps
;; ~~~~~~~
(setq reb-re-syntax 'string)


;; Scrolling
;; ~~~~~~~~~
;; Make scrolling behave like it does in VIM.
(setq scroll-conservatively 10000
      scroll-margin 0
      scroll-step 1)


;; Misc
;; ~~~~
;; Enable git-gutter.
(global-git-gutter-mode t)

;; Hide gutter if there are no changes.
(setq git-gutter:hide-gutter t)


(provide 'init-core)
