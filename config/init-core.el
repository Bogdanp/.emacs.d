;;; init-core.el --- miscellaneous configuration file
;;; Commentary:
;;; Code:
;;; Backups
(defvar local-temp-dir)
(setq auto-save-file-name-transforms `((".*"   ,local-temp-dir t))
      backup-directory-alist         `((".*" . ,local-temp-dir))
      backup-by-copying t)


;;; Compilation
;; Follow compilation output.
(require 'compile)
(setq compilation-scroll-output t)

;; Make it easier to compile shit in one key press.
(defconst bp-compile-with-default-command--buffer-name "*bp-default-compilation*"
  "The name of the default-command-compilation buffer.")
(defconst bp-compile-with-default-command--buffer-delay 0.25
  "How long to wait until successful compilation buffers are closed.")

(defvar bp-compile-with-default-command--command nil
  "The current compilation command.

  The user is prompted for a command when this is `nil`.")

(defun bp-compile-with-default-command--finish-hook (buffer string)
  "Hides BUFFER if STRING is 'finished' and there were no warnings."
  (when (and (string-match "finished" string)
             (string-equal (buffer-name buffer)
                           bp-compile-with-default-command--buffer-name)
             (not (with-current-buffer buffer
                    (goto-char 1)
                    (search-forward "warning" nil t))))
    (run-with-timer bp-compile-with-default-command--buffer-delay nil
                    (lambda (buffer)
                      (bury-buffer buffer)
                      (switch-to-prev-buffer (get-buffer-window buffer) 'kill))
                    buffer)))

(defun bp-compile-with-default-command--impl ()
  "Handle compilation with default command."
  (let* ((compilation-buffer-name-function
          (lambda (_)
            bp-compile-with-default-command--buffer-name)))
    (compile bp-compile-with-default-command--command)
    (add-hook 'compilation-finish-functions
              #'bp-compile-with-default-command--finish-hook)))

(defun bp-compile-with-default-command ()
  "Compile with the default command."
  (interactive)
  (if bp-compile-with-default-command--command
      (bp-compile-with-default-command--impl)
    (setq bp-compile-with-default-command--command
          (read-string "Compilation command: "))
    (bp-compile-with-default-command--impl)))

(defun bp-compile-with-default-command-reset ()
  "Reset the default compilation command."
  (interactive)
  (setq bp-compile-with-default-command--command nil)
  (bp-compile-with-default-command))


;;; Editing
;; Never use tabs.
(setq-default indent-tabs-mode nil)

;; Highlight current line.
(define-global-minor-mode my-global-hl-line-mode global-hl-line-mode
  (lambda ()
    "You can't turn off global-hl-line-mode on a per-buffer basis so we
can just build up our own version that doesn't activate for a given list
of modes."
    (when (not (memq major-mode (list 'eww-mode
                                      'term-mode
                                      'org-agenda-mode)))
      (hl-line-mode +1))))

(my-global-hl-line-mode)

;; Don't wrap long lines.
(setq-default truncate-lines t)

;; Highlight matching parens.
(show-paren-mode +1)

;; Fuck electric-indent-mode.
(electric-indent-mode +1)

;; Prefer utf-8.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Make fill-paragraph more useful.
(setq sentence-end-double-space nil)

;; Highlight TODOs.
(defun my-hl-todos ()
  "Highlight TODO items in comments."
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\|NOTE\\|XXX\\):" 1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook #'my-hl-todos)


;;; Files
;; Delete trailing whitespaces whenever a file gets saved.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Make default dired slightly nicer.
(setq insert-directory-program "/usr/local/bin/gls")
(setq dired-listing-switches "--group-directories-first -alh")


;;; Me
(setq user-full-name "Bogdan Popa")
(setq user-mail-address "popa.bogdanp@gmail.com")


;;; Modeline
;; Show current (row, col) in modeline.
(line-number-mode +1)
(column-number-mode +1)


;;; Regexps
(require 're-builder)
(setq reb-re-syntax 'string)


;;; Scrolling
;; Make scrolling behave like it does in VIM.
(setq scroll-margin 0
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Improved scrolling when using the trackpad.
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))


;;; UI
;; Use y and n instead of yes and no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; No bell of any kind.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()))

;; Disable tooltips.
(tooltip-mode -1)

;; Prevent the cursor from blinking.
(blink-cursor-mode -1)

;; Pretty terminal colors!!
(unless (display-graphic-p)
  (load-theme 'wombat t))


;;; Windows
(defvar bp-window-previous-window-configuration nil
  "Holds the previous window configuration.")

(defun bp-window-toggle-fullscreen ()
  "Toggle between whether or not the current window should be maximized."
  (interactive)
  (if bp-window-previous-window-configuration
      (progn
	(set-window-configuration bp-window-previous-window-configuration)
	(setq bp-window-previous-window-configuration nil))
    (progn
      (setq bp-window-previous-window-configuration (current-window-configuration))
      (delete-other-windows))))


(provide 'init-core)
;;; init-core.el ends here
