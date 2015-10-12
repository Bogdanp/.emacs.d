;;; init.el --- main config entry point -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;;; Misc. builtin options
(setq
 ;;; Me
 user-full-name "Bogdan Popa"
 user-mail-address "popa.bogdanp@gmail.com"

 ;;; GC
 ;; EMACS' default GC threshold is <1MB. Give it 200MB instead.
 gc-cons-threshold 200000000

 ;;; auto-compile
 load-prefer-newer t)

(setq-default
 ;;; Editing
 ;; Never use tabs.
 indent-tabs-mode nil

 ;; Wrap long lines
 truncate-lines nil)


;;; Vendored libs
(add-to-list 'load-path (locate-user-emacs-file "vendor/dash"))
(add-to-list 'load-path (locate-user-emacs-file "vendor/packed"))
(add-to-list 'load-path (locate-user-emacs-file "vendor/auto-compile"))
(add-to-list 'load-path (locate-user-emacs-file "vendor/use-package"))
(add-to-list 'load-path (locate-user-emacs-file "private"))


;;; auto-compile
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)


;;; use-package
(require 'use-package)


;;; UI
;; Position and resize frame.
(when (window-system)
  (add-to-list 'default-frame-alist '(font . "Fira Mono-12"))
  (add-to-list 'default-frame-alist '(top . 32))
  (add-to-list 'default-frame-alist '(left . 10))
  (add-to-list 'default-frame-alist '(width . 199))
  (add-to-list 'default-frame-alist '(height . 59)))

;; Remove GUI elements.
(dolist (mode '(blink-cursor-mode
		menu-bar-mode
		tool-bar-mode
		tooltip-mode
		scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(setq
 ;; Disable welcome screen.
 inhibit-startup-message t

 ;; No bell of any kind.
 ring-bell-function (lambda ())
 visible-bell nil

 ;; Make scrolling behave like it does in VIM.
 scroll-margin 0
 scroll-step 1
 scroll-conservatively 10000
 scroll-preserve-screen-position 1

 ;; Improved scrolling when using the trackpad.
 mouse-wheel-follow-mouse 't
 mouse-wheel-scroll-amount '(1 ((shift) . 1)))


;; Use y and n instead of yes and no.
(defalias 'yes-or-no-p 'y-or-n-p)


;;; Modeline
;; Show current (row, col) in modeline.
(column-number-mode +1)
(line-number-mode +1)


;;; Paths
;; Home sweet home.
(setq default-directory (expand-file-name "~/"))

(defconst local-temp-dir (expand-file-name (locate-user-emacs-file "temp"))
  "The folder in which temp files should be stored.")


;;; Packages
(with-no-warnings
  (require 'cl)
  (require 'package))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


;;; Themes
(defun bp-remove-themes ()
  "Remove all of the themes that are currently enabled."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))

(defun bp-load-theme ()
  "Load a theme interactively, removing all other themes first."
  (interactive)
  (bp-remove-themes)
  (call-interactively #'load-theme))

(if (display-graphic-p)
    (progn
      (use-package server
	:unless server-running-p
	:config (server-start))

      (use-package twilight-bright-theme
	:disabled t
	:ensure t
	:config (load-theme 'twilight-bright t))

      ;;; Disabled
      (use-package twilight-anti-bright-theme
	:load-path "vendor/twilight-anti-bright-theme"
	:config (load-theme 'twilight-anti-bright t))

      (use-package better-default-theme
	:disabled t
	:load-path "vendor/better-default-theme"
	:config (load-theme 'better-default t)))

  (load-theme 'wombat t))


;;; Keybindings
(use-package bind-key
  :init
  (bind-keys ("C-j" . newline-and-indent)
	     ("C-w" . backward-kill-word)
	     ("C--" . text-scale-decrease)
	     ("C-=" . text-scale-increase)
	     ("C-+" . text-scale-increase)))


;;; EVIL
(use-package evil
  :load-path "vendor/evil"
  :pin manual
  :preface
  (progn
    (defun bp-default-to-emacs-mode-hook ()
      (evil-emacs-state))

    (defun bp-toggle-emacs-mode-hook ()
      (if (equal evil-state 'emacs)
	  (evil-normal-state)
	(evil-emacs-state)))

    (defun bp-minibuffer-setup-hook-for-evil ()
      (local-set-key (kbd "C-w") 'backward-kill-word))

    (defun bp-evil-local-mode-hook ()
      (setq-local interprogram-cut-function nil)
      (setq-local interprogram-paste-function nil)))
  :config
  (progn
    ;;; Dependencies
    (use-package goto-chg
      :commands goto-last-change
      :ensure t)

    (use-package undo-tree
      :diminish undo-tree-mode
      :ensure t
      :commands (global-undo-tree-mode)
      :init
      (add-hook 'after-init-hook #'global-undo-tree-mode)
      :config
      (progn
	(with-no-warnings
	  (setq undo-tree-visualizer-timestamps t
		undo-tree-visualizer-diffs t
		undo-tree-history-directory-alist `((".*" . ,local-temp-dir))
		undo-tree-auto-save-history t))))


    ;;; Plugins
    (use-package evil-surround
      :ensure t
      :config
      (add-hook 'evil-mode-hook #'global-evil-surround-mode))

    (use-package evil-commentary
      :load-path "vendor/evil-commentary"
      :diminish evil-commentary-mode
      :config
      (add-hook 'evil-mode-hook #'evil-commentary-mode))

    (use-package evil-indent-textobject
      :load-path "vendor/evil-indent-textobject")

    (use-package evil-visual-mark-mode
      :load-path "vendor/evil-visual-mark-mode"
      :config
      (add-hook 'evil-mode-hook #'evil-visual-mark-mode))

    (use-package evil-jumper
      :load-path "vendor/evil-jumper"
      :config
      (add-hook 'evil-mode-hook #'global-evil-jumper-mode))

    (use-package evil-visualstar
      :load-path "vendor/evil-visualstar"
      :config
      (progn
	(setq evil-visualstar/persistent t)

	(add-hook 'evil-mode-hook #'global-evil-visualstar-mode)))


    ;;; Fixes
    ;; Default to EMACS mode in these modes.
    (dolist (mode '(calendar-mode
		    cider-docview-mode
		    cider-macroexpansion-mode
		    cider-popup-buffer-mode
		    cider-repl-mode
		    cider-stacktrace-mode
		    comint-mode
		    compilation-mode
		    debugger-mode
		    diff-mode
		    dired-mode
		    elm-interactive-mode
		    elm-package-mode
		    erc-mode
		    eshell-mode
                    geiser-repl-mode
		    git-rebase-mode
		    grep-mode
		    haskell-interactive-mode
		    help-mode
		    inferior-python-mode
		    Info-mode
		    macrostep-mode
		    magit-mode
		    magit-blame-mode
		    magit-cherry-mode
		    magit-diff-mode
		    magit-log-mode
		    magit-log-select-mode
		    magit-popup-mode
		    magit-popup-help-mode
		    magit-popup-sequence-mode
		    magit-reflog-mode
		    magit-refs-mode
		    magit-revision-mode
		    magit-stash-mode
		    magit-stashes-mode
		    magit-status-mode
		    message-mode
		    monky-mode
		    special-mode
		    paradox-commit-list-mode
		    paradox-menu-mode
		    process-menu-mode
		    prodigy-mode
		    sbt-mode
		    term-mode
		    undo-tree-visualizer-mode
		    utop-mode))
      (evil-set-initial-state mode 'emacs))

    ;; Default to EMACS mode whenever these hooks are invoked.
    (dolist (hook '(flycheck-error-list-mode-hook
		    git-commit-setup-hook
		    git-timemachine-mode-hook))
      (add-hook hook #'bp-default-to-emacs-mode-hook))

    ;; Toggle between emacs mode whenever these hooks are invoked.
    (dolist (hook '(magit-blame-mode-hook))
      (add-hook hook #'bp-toggle-emacs-mode-hook))

    ;; Make C-w work in the minibuffer.
    (add-hook 'minibuffer-setup-hook #'bp-minibuffer-setup-hook-for-evil)

    ;; Fix clipboard dirtying.
    (add-hook 'evil-local-mode-hook #'bp-evil-local-mode-hook)

    ;; Fix copy-on-motion.
    (defadvice evil-visual-update-x-selection (around clobber-x-select-text activate)
      (fset 'old-x-select-text (symbol-function 'x-select-text))
      (fmakunbound 'x-select-text)
      ad-do-it
      (fset 'x-select-text (symbol-function 'old-x-select-text)))


    ;;; Bindings
    ;; "localleader"
    (bind-keys :map evil-normal-state-map
	       ;; Misc
	       (",," . evil-ex-nohighlight)
	       (",x" . calc)
	       (",v" . set-selective-display))

    ;; NORMAL mode
    (bind-keys :map evil-normal-state-map
	       ("C-a" . beginning-of-line)
	       ("C-e" . end-of-line))

    ;; INSERT mode
    (bind-keys :map evil-insert-state-map
	       ("C-a" . beginning-of-line)
	       ("C-e" . end-of-line))

    ;; VISUAL mode
    (bind-keys :map evil-visual-state-map
	       ("C-a" . beginning-of-line)
	       ("C-e" . end-of-line))

    (evil-mode +1)))


;;; Builtins
(use-package autorevert
  :config
  ;; Revert files that update on disk automatically. Ignores dirty
  ;; buffers.
  (global-auto-revert-mode))

(use-package compile
  :init
  (setq compilation-scroll-output t))

(use-package dired
  :commands dired
  :init
  (setq insert-directory-program "/usr/local/bin/gls"
	dired-listing-switches "--group-directories-first -alh")
  :config
  (use-package dired+
    :ensure t))

(use-package electric
  :config
  (electric-indent-mode +1))

(use-package erc
  :commands erc
  :init
  (setq
   ;; Default config.
   erc-server "irc.freenode.net"
   erc-port 6667
   erc-nick "bogdanp"
   erc-user-full-name user-full-name

   ;; Highlight these things in incoming messages.
   erc-keywords '("bogdanp")

   ;; Autojoin these channels on freenode.
   erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#erc" "#haskell" "#python" "#scala"
				  "#purescript" "#pixie-lang" "#elm"))

   ;; Behave like a "normal" IRC client.
   erc-kill-buffer-on-part t
   erc-kill-queries-on-quit t
   erc-kill-server-buffer-on-quit t))

(use-package etags
  :init
  ;; Never append tags lists together.
  (setq tags-add-tables nil))

(use-package ffap
  :commands ffap-other-window)

(use-package files
  :init
  (setq auto-save-file-name-transforms `((".*" ,(concat local-temp-dir "/\\1") t))
	backup-directory-alist         `((".*" . ,local-temp-dir))
	backup-by-copying t))

(use-package grep
  :config
  (progn
    ;; Fish compatibility
    (grep-apply-setting
     'grep-find-command '("find . -type f -exec grep -nH -e  \\{\\} \\+" . 34))
    (grep-apply-setting
     'grep-find-template "find . <X> -type f <F> -exec grep <C> -inH -e <R> \\{\\} \\+")))

(use-package hippie-expand
  :bind (("M-/" . hippie-expand)))

(use-package hl-line
  :config
  (progn
    (define-global-minor-mode bp-global-hl-line-mode global-hl-line-mode
      (lambda ()
	;; XXX: You can't turn off global-hl-line-mode on a per-buffer
	;; basis so we can just build up our own version that doesn't
	;; activate for a given list of modes.
	(when (not (memq major-mode (list 'eww-mode
					  'term-mode
					  'org-agenda-mode)))
	  (hl-line-mode +1))))

    (bp-global-hl-line-mode)))

(use-package ido
  :init
  (setq ido-create-new-buffer 'always
	ido-use-filename-at-point 'guess
	ido-use-virtual-buffers t
	ido-handle-duplicate-virtual-buffers 2
	ido-max-prospects 10
	ido-ignore-extensions t)
  :config
  (progn
    (use-package ido-ubiquitous
      :ensure t
      :config
      (ido-ubiquitous-mode +1))

    (use-package ido-vertical-mode
      :disabled t
      :ensure t
      :init
      (setq ido-vertical-show-count t)
      :config
      (progn
	(ido-vertical-mode +1)))

    (use-package ido-clever-match
      :load-path "vendor/ido-clever-match"
      :config
      (ido-clever-match-enable))

    (ido-mode +1)
    (ido-everywhere +1)))

(use-package mule
  :config
  (progn
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package re-builder
  :commands re-builder
  :init
  (setq reb-re-syntax 'rx))

(use-package recentf
  :init
  (setq recentf-save-file (locate-user-emacs-file "recentf")
	recentf-max-saved-items 1000
	recentf-max-menu-items 10
	recentf-auto-cleanup 60)
  :config
  (progn
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
    (add-to-list 'recentf-exclude "MERGE_MSG\\'")

    (recentf-mode +1)))

(use-package savehist
  :init
  (setq savehist-file (locate-user-emacs-file "savehist")
	savehist-additional-variables '(search ring regexp-search-ring)
	savehist-autosave-interval 60

	history-length 10000)
  :config
  (savehist-mode +1))

(use-package saveplace
  :init
  (setq-default save-place t))

(use-package simple
  :init
  ;; Delete trailing whitespace whenever a file gets saved.
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(use-package smtpmail
  :init
  (setq starttls-use-gnutls t

	send-mail-function 'smtpmail-send-it
	message-send-mail-function 'smtpmail-send-it

	smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	smtpmail-auth-credentials (expand-file-name "~/.authinfo")
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587
	smtpmail-debug-info t))

(use-package term
  :init
  (defadvice term-line-mode (after enable-hl-line-in-term-line-mode)
    (hl-line-mode 1)
    (evil-normal-state))

  (defadvice term-char-mode (after disable-hl-line-in-term-char-mode)
    (hl-line-mode 0)
    (evil-emacs-state))
  :config
  (progn
    ;;; Zipper
    (cl-defstruct zipper lhs curr rhs)

    (defun zipper-append (zipper x)
      "Append to ZIPPER the value of X."
      (setf (zipper-rhs zipper)
	    (reverse (cons x (reverse (zipper-rhs zipper))))))

    (defun zipper-drop (zipper)
      "Drop the current element from ZIPPER."
      (setf (zipper-curr zipper) nil)
      (zipper-next zipper))

    (defun zipper-beginning (zipper)
      "Goto the beginning of ZIPPER."
      (setf (zipper-rhs zipper)
	    (append (reverse (cons (zipper-curr zipper)
				   (zipper-lhs zipper)))
		    (zipper-rhs zipper)))
      (setf (zipper-curr zipper) nil)
      (setf (zipper-lhs zipper) nil))

    (defun zipper-end (zipper)
      "Goto the end of ZIPPER."
      (setf (zipper-lhs zipper)
	    (append (reverse (cons (zipper-curr zipper)
				   (zipper-rhs zipper)))
		    (zipper-lhs zipper)))
      (setf (zipper-rhs zipper) nil)
      (setf (zipper-curr zipper)
	    (car (zipper-lhs zipper)))
      (setf (zipper-lhs zipper)
	    (cdr (zipper-lhs zipper))))

    (defmacro defmover (name f g)
      "Define a zipper modifier function called NAME.

      F is where data gets moved to.
      G is where data gets moved from."
      `(defun ,name (zipper)
	 (when (funcall ,f zipper)
	   (let ((x  (car (funcall ,f zipper)))
		 (xs (cdr (funcall ,f zipper))))

	     (when (zipper-curr zipper)
	       (setf (,(cadr g) zipper)
		     (cons (zipper-curr zipper)
			   (funcall ,g zipper))))

	     (setf (zipper-curr zipper) x)
	     (setf (,(cadr f) zipper) xs)))
	 (zipper-curr zipper)))

    (defmover zipper-next #'zipper-rhs #'zipper-lhs)
    (defmover zipper-prev #'zipper-lhs #'zipper-rhs)

    ;;; Term
    (require 'ansi-color)

    (defconst bp-term-shell "fish"
      "The path to the shell that should be run.")

    (defvar bp-term-previous-window-configuration nil
      "Holds the previous window configuration.")

    (defvar bp-term-current-term-buffer nil
      "Holds the current term buffer.")

    (defvar bp-term-terms
      (make-zipper :lhs  nil
		   :rhs  nil
		   :curr nil)
      "A zipper for all of the existing terms.")

    (defun bp-maybe-switch-to-buffer (buffer)
      "Switch to BUFFER iff it is non-nil."
      (when buffer
	(switch-to-buffer buffer)))

    (defun bp-term-add ()
      "Add a new terminal and jump to it."
      (interactive)
      (zipper-end bp-term-terms)
      (zipper-append bp-term-terms (ansi-term bp-term-shell))
      (bp-term-next))

    (defun bp-term-kill ()
      "Kill the current terminal."
      (interactive)
      (when (>= (length (zipper-rhs bp-term-terms)) 1)
	(let ((buffer (zipper-drop bp-term-terms)))
	  (kill-buffer bp-term-current-term-buffer)
	  (setq bp-term-current-term-buffer buffer)
	  (bp-maybe-switch-to-buffer buffer))))

    (defun bp-term-next ()
      "Goto the next terminal in the zipper."
      (interactive)
      (let ((buffer (zipper-next bp-term-terms)))
	(setq bp-term-current-term-buffer buffer)
	(bp-maybe-switch-to-buffer buffer)))

    (defun bp-term-prev ()
      "Goto the previous terminal in the zipper."
      (interactive)
      (let ((buffer (zipper-prev bp-term-terms)))
	(setq bp-term-current-term-buffer buffer)
	(bp-maybe-switch-to-buffer buffer)))

    (defun bp-term-fullscreen ()
      "Make the term fullscreen."
      (setq bp-term-previous-window-configuration (current-window-configuration))
      (delete-other-windows)
      (if bp-term-current-term-buffer
	  (bp-maybe-switch-to-buffer bp-term-current-term-buffer)
	(bp-term-add)
	(setq bp-term-current-term-buffer (zipper-curr bp-term-terms))))

    (defun bp-term-toggle ()
      "Toggle between the current window config and a terminal."
      (interactive)
      (if bp-term-previous-window-configuration
	  (progn
	    (set-window-configuration bp-term-previous-window-configuration)
	    (setq bp-term-previous-window-configuration nil))
	(bp-term-fullscreen)))

    (defun bp-term-clipboard-paste ()
      "Paste the contents of the clipboard into the current term."
      (interactive)
      (term-send-raw-string (or (if (fboundp #'bp-clipboard-value)
				    (bp-clipboard-value)
				  (x-get-clipboard))
				"")))

    ;;; Server
    (defun bp-server-visit-hook-for-term ()
      "Most of the time I call `emacsclient' I'll be toggled-into `bp-term-**'.

      I don't want calling `emacsclient' to break that configuration
      so this hook works around that by toggling out of that
      configuration before switching to the new buffer."
      (let ((buffer (current-buffer)))
	(when bp-term-previous-window-configuration
	  (bp-term-toggle)
	  (switch-to-buffer buffer))))

    (add-hook 'server-visit-hook #'bp-server-visit-hook-for-term)

    (bind-keys :map term-raw-escape-map
	       ("c"    . bp-term-add)
	       ("\C-k" . bp-term-kill)
	       ("\C-n" . bp-term-next)
	       ("\C-p" . bp-term-prev)
	       ("\C-y" . bp-term-clipboard-paste))

    (bind-keys ("C-c M-a" . bp-term-toggle))))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package winner
  :config
  (progn
    (winner-mode +1)

    (bind-keys :map evil-normal-state-map
               (",ww" . winner-undo)
               (",wr" . winner-redo))))


;;; Buffers and buffer navigation
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :preface
  (progn
    (eval-when-compile
      (declare-function ibuffer-do-sort-by-alphabetic "ibuf-ext"))

    (defun bp-ibuffer-hook ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
	(ibuffer-do-sort-by-alphabetic))))
  :config
  (progn
    (use-package ibuffer-vc
      :commands ibuffer-vc-set-filter-groups-by-vc-root
      :ensure t)

    (add-hook 'ibuffer-hook #'bp-ibuffer-hook)))

(use-package imenu
  :bind ("C-x C-i" . imenu))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
	 ("C-;" . smex))
  :ensure t
  :init
  (setq smex-save-file (locate-user-emacs-file ".smex-items"))
  :config
  (smex-initialize))


;;; UI
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  :config
  (progn
    (sml/setup)

    (add-to-list 'sml/replacer-regexp-list '("^~/Work/" ":W:") t)
    (add-to-list 'sml/replacer-regexp-list '("^~/sandbox/" ":s:") t)))


;;; Git
(use-package magit
  :ensure t
  :commands (magit-status git-commit-mode)
  :mode (("COMMIT_EDITMSG\\'" . git-commit-mode)
	 ("MERGE_MSG"         . git-commit-mode))
  :bind ("C-c m" . magit-status)
  :init
  (setq magit-revert-buffers t
	magit-completing-read-function #'magit-ido-completing-read
	magit-last-seen-setup-instructions "1.4.0"
	magit-push-always-verify nil)
  :config
  (use-package fullframe
    :ensure t
    :config
    (fullframe magit-status magit-mode-quit-window)))

(use-package git-timemachine
  :commands git-timemachine
  :ensure t)


;;; Org
(use-package org
  :commands org-mode
  :ensure t
  :defer 2
  :preface
  (progn
    (eval-when-compile
      (declare-function org-cut-subtree "org")
      (declare-function org-end-of-subtree "org")
      (declare-function org-goto-first-child "org")
      (declare-function org-goto-sibling "org")
      (declare-function org-paste-subtree "org")
      (declare-function outline-up-heading "outline"))

    ;;; Archiving
    (defun bp-org-level-of-heading-at-point ()
      "Returns the level of the headline at point."
      (length (car (split-string (thing-at-point 'line t) " "))))

    (defun bp-org-archive-task-at-point ()
      "Moves the task at point into the first heading of its parent
    (which, by convention, should be an Archive heading)."
      (interactive)
      (save-excursion
	(let ((start-level (bp-org-level-of-heading-at-point)))
	  (org-cut-subtree)

	  ;; Cutting the subtree might place us on a different level.
	  ;; Account for those cases.
	  (let ((current-level (bp-org-level-of-heading-at-point)))
	    (if (< current-level start-level)
		(progn
		  (org-goto-sibling 'previous)
		  (dotimes (number (- start-level current-level 1))
		    (org-end-of-subtree)
		    (org-goto-first-child)))
	      (outline-up-heading (+ 1 (- current-level start-level)))))

	  ;; TODO: Turn this into a heading search?
	  (org-goto-first-child)

	  (let ((archive-level (bp-org-level-of-heading-at-point)))
	    (forward-line)
	    (org-paste-subtree (+ 1 archive-level)))))))
  :config
  (progn
    ;;; Misc
    ;; Paths to my org files
    (defvar bp-org-dir (expand-file-name "~/Dropbox/Documents/Personal"))
    (defvar bp-org-main-file (expand-file-name (concat bp-org-dir "/Bogdan.org")))
    (defvar bp-org-journal-file (expand-file-name (concat bp-org-dir "/Journal.org")))


    ;;; Completion
    (setq org-completion-use-ido t
          org-outline-path-complete-in-steps nil)


    ;;; Code blocks
    ;; Highlight code in BEGIN_SRC-END_SRC blocks.
    (setq org-src-fontify-natively t)


    ;;; Babel
    ;; Allow these languages to be executed in org code blocks.
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((haskell . t)
       (latex   . t)
       (python  . t)
       (sh      . t)
       (dot     . t)))

    ;; Make org-babel work w/ these languages.
    (require 'ob-haskell)
    (require 'ob-latex)


    ;; Evaluate code in org files w/o asking for confirmation.
    ;; Potentially dangerous but meh.
    (setq org-confirm-babel-evaluate nil)


    ;;; Capture
    ;; Where to put captured stuff.
    (setq org-default-notes-file bp-org-main-file)

    ;; Capture templates.
    (use-package bp-org-capture-templates)


    ;;; Agenda
    ;; Set up path to agenda files.
    (defvar bp-org-agenda-files-path bp-org-dir)
    (when (file-exists-p bp-org-agenda-files-path)
      (setq org-agenda-files `(,bp-org-agenda-files-path)))

    ;; Custom commands for easy filtering.
    (use-package bp-org-agenda-commands)


    ;;; TODOs
    (setq
     ;; Log the closing time of TODO items.
     org-log-done 'time

     ;; Better todo states.
     org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))

     ;; Refile anywhere.
     org-refile-targets '((nil :maxlevel . 9)))


    ;;; Reminders
    ;; Code below mostly stolen from http://doc.norang.ca/org-mode.html#Reminders
    (defun bp-org-agenda-to-appt ()
      "Erase all current reminders and rebuild the list from the current agenda."
      (interactive)
      (setq appt-time-msg-list nil)
      (org-agenda-to-appt))

    (setq
     ;; Plz don't ruin my window setup, org-agenda.
     org-agenda-window-setup 'current-window

     ;; Display appointment info in the modeline.
     appt-display-mode-line t
     appt-display-format 'echo)

    ;; Rebuild reminders each time the agenda is displayed.
    (add-hook 'org-finalize-agenda-hook #'bp-org-agenda-to-appt 'append)

    ;; Activate appointments.
    (appt-activate t)

    ;; Reset appointments 1 minute after midnight.
    (run-at-time "24:01" nil #'bp-org-agenda-to-appt)

    ;; Setup appointments at startup.
    (bp-org-agenda-to-appt)


    ;;; Text editing
    (add-hook 'org-mode-hook #'auto-fill-mode)


    ;;; Bindings
    (bind-keys :map evil-normal-state-map
	       (",a"  . org-agenda)
	       (",c"  . org-capture)
	       (",ta" . bp-org-archive-task-at-point))))


;;; Code completion
(use-package auto-complete
  :commands (ac-define-source auto-complete-mode)
  :diminish auto-complete-mode
  :ensure t
  :init
  ;; Auto-complete all the programming.
  (add-hook 'prog-mode-hook #'auto-complete-mode)
  :config
  (progn
    ;; Load AC's default configs.
    (require 'auto-complete-config)

    (ac-config-default)
    (ac-set-trigger-key "TAB")

    ;; Source ALL THE THINGS.
    (setq-default ac-sources '(ac-source-filename
			       ac-source-imenu
			       ac-source-features
			       ac-source-abbrev
			       ac-source-words-in-same-mode-buffers
			       ac-source-dictionary
			       ac-source-yasnippet))

    (setq ac-auto-start 5
	  ac-auto-show-menu 1
	  ac-quick-help-delay 1

	  ac-use-menu-map t
	  ac-use-fuzzy nil
	  ac-use-quick-help t)))

(use-package company
  :commands company-mode
  :diminish company-mode
  :ensure t
  :init
  (setq company-idle-delay 0.25))

(use-package yasnippet
  :commands (yas-minor-mode yas-reload-all)
  :diminish yas-minor-mode
  :ensure t
  :config
  (yas-reload-all))


;;; Linting
(use-package flycheck
  :commands flycheck-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (progn
    (setq-default flycheck-disabled-checkers '(haskell-ghc
					       html-tidy
					       javascript-jshint))

    (flycheck-add-mode 'javascript-eslint 'web-mode)))


;;; File navigation
(use-package projectile
  :diminish projectile-mode
  :load-path "vendor/smex"
  :ensure t
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode))


;;; Package management
(use-package paradox
  :commands paradox-list-packages
  :ensure t
  :config
  (setq paradox-github-token t))


;;; Process management
(use-package prodigy
  :bind (("C-c P" . prodigy))
  :ensure t
  :preface
  (defun bp-prodigy-toggle-compilation-mode ()
    (interactive)
    (if (eq major-mode 'compilation-mode)
	(prodigy-view-mode)
      (compilation-mode))
    (if (fboundp #'bp-prodigy-view-mode-hook)
	(bp-prodigy-view-mode-hook))
    (goto-char (point-max)))

  (defun bp-prodigy-view-mode-hook ()
    (bind-key "C-c C-t" 'bp-prodigy-toggle-compilation-mode))

  :config
  (progn
    (add-hook 'prodigy-view-mode-hook #'bp-prodigy-view-mode-hook)

    (use-package bp-prodigy-services)))


;;; Miscellaneous
(use-package dash-at-point
  :ensure t
  :config
  (bind-keys :map evil-normal-state-map
	     (",d" . dash-at-point)))

(use-package diminish
  :commands diminish
  :ensure t)

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :commands exec-path-from-shell-initialize
    :ensure t
    :init
    (add-hook 'after-init-hook #'exec-path-from-shell-initialize)))

(use-package hl-todo
  :ensure t
  :config
  (progn
    (setq hl-todo-activate-in-modes '(c-mode
				      emacs-lisp-mode
				      elm-mode
				      haskell-mode
				      python-mode
				      scala-mode))

    (global-hl-todo-mode)))


;;; C
(use-package cc-mode
  :mode ("\\.c\\'" . c-mode)
  :config
  (progn
    (setq c-default-style "bsd"
	  c-basic-offset 4)

    ;; Fix indentation.
    (defun bp-c-mode-hook ()
      (c-set-offset 'arglist-intro '+))

    (use-package irony
      :ensure t
      :commands irony-mode
      :preface
      (progn
	(defun bp-irony-mode-hook ()
	  ;; Disable AC since its irony mode isn't ready yet.
	  (auto-complete-mode -1)

	  (eldoc-mode +1)
	  (irony-eldoc +1)
	  (company-mode +1)))
      :config
      (progn
	(use-package company-irony
	  :ensure t
	  :preface
	  (progn
	    (defun bp-company-irony-setup-hook ()
	      (add-to-list 'company-backends 'company-irony)))
	  :init
	  (progn
	    (add-hook 'irony-mode-hook #'bp-company-irony-setup-hook)
	    (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)))

	(use-package flycheck-irony
	  :ensure t
	  :preface
	  (progn
	    (defun bp-flycheck-irony-setup-hook ()
	      (add-to-list 'flycheck-checkers 'irony)))
	  :init
	  (add-hook 'irony-mode-hook #'bp-flycheck-irony-setup-hook))

	(use-package irony-eldoc
	  :commands irony-eldoc
	  :ensure t)

	(add-hook 'irony-mode-hook #'bp-irony-mode-hook)))

    (add-hook 'c-mode-hook #'bp-c-mode-hook)
    (add-hook 'c-mode-hook #'irony-mode)))


;;; Clojure
(use-package clojure-mode
  :ensure t
  :mode (("\\.cljs?\\'" . clojure-mode)
	 ("\\.boot\\'"  . clojure-mode))
  :config
  (add-hook 'clojure-mode-hook #'cider-mode))

(use-package cider
  :ensure t
  :commands (cider-mode)
  :preface
  (defun my-cider-mode-hook ()
    (auto-complete -1))
  :config
  (progn
    (add-hook 'cider-repl-mode-hook #'my-cider-mode-hook)
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-repl-mode-hook #'paredit-mode)
    (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'cider-mode-hook #'my-cider-mode-hook)
    (add-hook 'cider-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'eldoc-mode)
    (add-hook 'cider-mode-hook #'paredit-mode)
    (add-hook 'cider-mode-hook #'rainbow-delimiters-mode)

    (bind-keys :map cider-mode-map
	       ("C-c ." . cider-jump-to-var)
	       ("C-c ," . cider-pop-back))))


;;; Docker
(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
  :ensure t)


;;; Elm
(use-package elm-mode
  :load-path "vendor/elm-mode"
  :mode ("\\.elm\\'" . elm-mode)
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-ac))


;;; Emacs lisp
(use-package eldoc
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package paredit
  :diminish paredit-mode
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))


;;; Fish
(use-package fish-mode
  :ensure t
  :mode "\\.fish\\'")


;;; Haskell
(use-package haskell-mode
  :mode "\\.l?hs\\'"
  :ensure t
  :preface
  (progn
    (defun bp-haskell-mode-hook ()
      ;; (set-face-attribute 'shm-current-face nil :background "#EEE")
      ;; (set-face-attribute 'shm-quarantine-face nil :background "#DDD")

      (setq-local indent-line-function #'indent-relative)))
  :config
  (progn
    (require 'haskell-interactive-mode)
    (require 'haskell-process)

    (use-package shm
      :ensure t
      :init
      (add-hook 'haskell-mode-hook #'structured-haskell-mode)
      :config
      (custom-set-variables
       '(shm-auto-insert-skeletons t)
       '(shm-auto-insert-bangs t)
       '(shm-use-presentation-mode t)))

    (use-package shakespeare-mode
      :disabled t
      :ensure t)

    (custom-set-variables
     ;; Haskell Process
     '(haskell-process-type 'stack-ghci)
     '(haskell-process-args-stack-ghci
       '("--ghc-options=-ferror-spans"
	 "--ghc-options=-fno-warn-name-shadowing"
	 "--ghc-options=-fno-warn-orphans"))

     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t)

     ;; Haskell Interactive
     '(haskell-interactive-mode-do-fast-keys t)
     '(haskell-interactive-mode-eval-pretty nil)
     '(haskell-interactive-mode-include-file-name nil)

     ;; Misc
     '(haskell-stylish-on-save t)
     '(haskell-notify-p t)
     '(haskell-tags-on-save t))

    (add-hook 'haskell-mode-hook #'haskell-doc-mode)
    (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode)
    (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
    (add-hook 'haskell-mode-hook #'bp-haskell-mode-hook)

    (bind-keys :map haskell-mode-map
	       ("C-c M-l" . haskell-process-reload-devel-main))))


;;; JSON
(use-package json-mode
  :mode "\\.json\\'"
  :ensure t)


;;; LaTeX
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . LaTeX-mode))


;;; LESS
(use-package less-css-mode
  :disabled t
  :mode "\\.less\\'"
  :ensure t
  :config
  (progn
    (defun bp-scss-mode-hook ()
      (setq-local css-indent-offset 2))

    (add-hook 'less-css-mode-hook 'bp-scss-mode-hook)))


;;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :ensure t)


;;; OCaml
(use-package tuareg
  :mode ("\\.mli?\\'" . tuareg-mode)
  :ensure t
  :config
  (progn
    (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var)))

    (use-package utop
      :ensure t)

    (use-package merlin
      :pin manual
      :load-path "~/.opam/system/share/emacs/site-lisp"
      :commands (merlin-mode)
      :config
      (progn
	(setq merlin-use-auto-complete-mode 'easy)
	(setq merlin-command 'opam)))

    (use-package ocp-indent
      :pin manual
      :load-path "~/.opam/system/share/emacs/site-lisp"
      :config
      (progn
	(setq ocp-indent-syntax '("lwt"))))

    (add-hook 'tuareg-mode-hook #'merlin-mode)
    (add-hook 'tuareg-mode-hook #'utop-minor-mode)))


;;; Python
(use-package python
  :mode (("\\.py\\'"   . python-mode)
	 ("SConstruct" . python-mode))
  :interpreter ("python" . python-mode)
  :preface
  (progn
    (eval-when-compile
      (declare-function py-test-define-project "py-test"))

    (defun bp-python-mode-hook ()
      (auto-complete-mode -1)))
  :config
  (progn
    (use-package elpy
      :commands (elpy-enable)
      :ensure t
      :init
      (with-eval-after-load 'python (elpy-enable))
      :config
      (progn
	(bind-keys :map python-mode-map
		   ("C-c v" . pyvenv-workon)
		   ("C-c ." . elpy-goto-definition)
		   ("C-c ," . pop-tag-mark))

	(custom-set-variables
	 '(elpy-modules
	   (quote
	    (elpy-module-company
	     elpy-module-eldoc
	     elpy-module-pyvenv
	     elpy-module-sane-defaults
	     elpy-module-yasnippet))))))

    (use-package py-test
      :ensure t
      :config
      (progn
	(evil-define-key 'normal python-mode-map
	  ",r" 'py-test-run-test-at-point
	  ",T" 'py-test-run-directory
	  ",t" 'py-test-run-file)

	;; Purty mode-line.
	(setq py-test-*mode-line-face-shenanigans-on* t)
	(setq py-test-*mode-line-face-shenanigans-timer* "0.5 sec")

	(use-package bp-py-test-projects)))

    (add-hook 'python-mode-hook #'bp-python-mode-hook)))


;;; REST
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :ensure t)


;;; Robot
(use-package robot-mode
  :load-path "vendor/robot-mode"
  :mode "\\.robot\\'")


;;; Scala
(use-package scala-mode2
  :mode (("\\.scala\\'" . scala-mode)
	 ("\\.sbt\\'"   . scala-mode))
  :ensure t)

(use-package ensime
  :commands ensime-scala-mode-hook
  :ensure t
  :preface
  (defun bp-scala-mode-hook ()
    (auto-complete-mode -1)
    (company-mode +1)

    (if (equal "build.sbt" (buffer-name))
	(flycheck-mode -1)))
  :init
  (progn
    (add-hook 'scala-mode-hook #'ensime-scala-mode-hook)
    (add-hook 'scala-mode-hook #'bp-scala-mode-hook))
  :config
  (progn
    (setq ensime-default-java-flags '("-Xms512M" "-Xmx1G")
	  ensime-sbt-command "activator")

    (let* ((faces ensime-sem-high-faces)
	   (faces (assq-delete-all 'implicitConversion faces))
	   (faces (assq-delete-all 'implicitParams faces)))
      (setq ensime-sem-high-faces faces))

    (bind-keys :map ensime-mode-map
	       ("C-c ." . ensime-edit-definition)
	       ("C-c ," . ensime-pop-find-definition-stack))))


;;; Scheme
(use-package geiser
  :ensure t
  :preface
  (defun bp-geiser-mode-hook ()
    (auto-complete-mode -1)
    (company-mode +1))
  :init
  (progn
    (add-hook 'geiser-mode-hook #'paredit-mode)
    (add-hook 'geiser-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'geiser-mode-hook #'bp-geiser-mode-hook)
    (setq geiser-active-implementations '(chicken))))


;;; SCSS
(use-package scss-mode
  :disabled t
  :mode "\\.scss\\'"
  :ensure t
  :config
  (progn
    ;; Stupid functionality is stupid.
    (setq scss-compile-at-save nil)

    (defun bp-scss-mode-hook ()
      (setq-local css-indent-offset 2))

    (add-hook 'scss-mode-hook 'bp-scss-mode-hook)))


;;; Terraform
(use-package terraform-mode
  :disabled t
  :ensure t
  :mode "\\.tf\\'")


;;; UrWeb
(use-package urweb-mode
  :disabled t
  :load-path "/usr/local/share/emacs/site-lisp/urweb-mode"
  :mode "\\.ur[ps]?\\'")


;;; Web
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
	 ("\\.php\\'"   . web-mode)
	 ("\\.hbs\\'"   . web-mode)
	 ("\\.jsx?\\'"  . web-mode))
  :init
  (setq web-mode-code-indent-offset 2
	web-mode-style-indent-offset 2
	web-mode-script-indent-offset 2
	web-mode-markup-indent-offset 2

	web-mode-style-padding 2
	web-mode-script-padding 2

	web-mode-enable-auto-closing t
	web-mode-enable-auto-expanding t
	web-mode-enable-auto-pairing t
	web-mode-enable-current-element-highlight t

	web-mode-engines-alist '(("razor"  . "\\.scala\\.html\\'")
				 ("django" . "\\.html\\'")))
  :config
  (progn
    (set-face-attribute 'web-mode-current-column-highlight-face nil :background "#EEE")
    (set-face-attribute 'web-mode-current-element-highlight-face nil :background "#EEE")))


;;; Yaml
(use-package yaml-mode
  :mode "\\.yaml\\'"
  :ensure t)


(provide 'init)
;;; init.el ends here
