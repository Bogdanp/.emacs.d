;;; init.el --- main config entry point -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;;; Misc. builtin options
(setq
 ;;; Me
 user-full-name "Bogdan Popa"
 user-mail-address "popa.bogdanp@gmail.com"

 ;;; GC
 ;; EMACS' default GC threshold is <1MB. Give it 8MB instead.
 gc-cons-threshold 8388608
 garbage-collection-messages nil

 ;;; auto-compile
 load-prefer-newer t

 ;;; Don't warn on redefinition
 ad-redefinition-action 'accept

 ;;; Don't attempt to load `default.el'
 inhibit-default-init t

 ;;; Mac port
 mac-option-modifier 'meta
 mac-command-modifier 'hyper
 mac-mouse-wheel-smooth-scroll t)

(setq-default
 ;;; Editing
 ;; Never use tabs.
 indent-tabs-mode nil

 ;; Don't wrap long lines
 truncate-lines t)

;;; Enable functionality
(put 'narrow-to-region 'disabled nil)

;;; Vendored libs
(add-to-list 'load-path (locate-user-emacs-file "vendor/dash"))
(add-to-list 'load-path (locate-user-emacs-file "vendor/packed"))
(add-to-list 'load-path (locate-user-emacs-file "vendor/auto-compile"))
(add-to-list 'load-path (locate-user-emacs-file "vendor/use-package"))
(add-to-list 'load-path (expand-file-name "~/Dropbox/Documents/Personal"))


;;; auto-compile
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)


;;; use-package
(require 'use-package)


;;; UI
;; Position and resize frame.
(when (window-system)
  (add-to-list 'default-frame-alist '(font . "inconsolata-14"))
  (add-to-list 'default-frame-alist '(top . 32))
  (add-to-list 'default-frame-alist '(left . 10))
  (add-to-list 'default-frame-alist '(width . 199))
  (add-to-list 'default-frame-alist '(height . 59))
  (add-to-list 'default-frame-alist '(fullscreen . fullboth)))

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

(defun bp-toggle-fullscreen ()
  "Toggle the current frame between fullscreen and not."
  (interactive)
  (let ((fullscreen (frame-parameter nil 'fullscreen)))
    (if (not fullscreen)
        (set-frame-parameter nil 'fullscreen 'fullboth)
      (set-frame-parameter nil 'fullscreen nil))))


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

(unless package-archive-contents
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

(if (not (display-graphic-p))
    (load-theme 'wombat t)

  (use-package server
    :unless server-running-p
    :config (server-start))

  (use-package twilight-bright-theme
    :disabled t
    :ensure t
    :config (load-theme 'twilight-bright t))

  (use-package twilight-anti-bright-theme
    :disabled t
    :load-path "vendor/twilight-anti-bright-theme"
    :config (load-theme 'twilight-anti-bright t))

  (use-package better-default-theme
    :load-path "vendor/better-default-theme"
    :config (load-theme 'better-default t)))


;;; Keybindings
(use-package bind-key
  :init
  (bind-keys ("C-j" . newline-and-indent)
             ("C-w" . backward-kill-word)
             ("C--" . text-scale-decrease)
             ("C-=" . text-scale-increase)
             ("C-+" . text-scale-increase)
             ("H-f" . bp-toggle-fullscreen)))


;;; EVIL
(use-package evil
  :load-path "vendor/evil"
  :pin manual
  :preface
  (defun bp-apply-evil-mode-hook ()
    (if (apply #'derived-mode-p '(fundamental-mode
                                  conf-mode css-mode
                                  evil-command-window-mode
                                  erlang-mode haskell-mode haskell-cabal-mode
                                  json-mode prog-mode purescript-mode
                                  restclient-mode rust-mode text-mode
                                  tuareg-mode web-mode
                                  yaml-mode))
        (evil-normal-state)
      (evil-emacs-state)))

  (defun bp-toggle-emacs-state ()
    (if (equal evil-state 'emacs)
        (evil-normal-state)
      (evil-emacs-state)))

  (defun bp-minibuffer-setup-hook ()
    (local-set-key (kbd "C-w") #'backward-kill-word))

  (defun bp-generate-mode-line-tag (f &rest state)
    (let ((tag (apply f state))
          (state (car state)))
      (cond
       ((string= state "emacs") (propertize tag 'face '((:background "red" :foreground "white"))))
       (t tag))))

  (defun bp-find-init-file ()
    (interactive)
    (find-file (locate-user-emacs-file "init.el")))

  (defun bp-open-iterm ()
    (interactive)
    (call-process-shell-command "open -a /Applications/iTerm.app"))
  :init
  (setq evil-search-module #'evil-search
        evil-magic 'very-magic)
  :config
  (progn
    ;;; Dependencies
    (use-package goto-chg
      :ensure t
      :commands goto-last-change)

    (use-package undo-tree
      :ensure t
      :diminish undo-tree-mode
      :commands global-undo-tree-mode
      :init
      (add-hook 'after-init-hook #'global-undo-tree-mode)
      :config
      (with-no-warnings
        (setq undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diffs t
              undo-tree-history-directory-alist `((".*" . ,local-temp-dir))
              undo-tree-auto-save-history t)))


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

    (use-package evil-jumper
      :load-path "vendor/evil-jumper"
      :config
      (add-hook 'evil-mode-hook #'global-evil-jumper-mode))

    (dolist (hook '(git-commit-setup-hook
                    git-timemachine-mode-hook
                    magit-blame-mode-hook
                    org-capture-mode-hook
                    org-log-buffer-setup-hook))
      (add-hook hook #'bp-toggle-emacs-state))

    (add-to-list 'evil-emacs-state-modes 'notmuch-tree-mode)

    (add-hook 'minibuffer-setup-hook #'bp-minibuffer-setup-hook)
    (add-hook 'after-change-major-mode-hook #'bp-apply-evil-mode-hook)
    (advice-add #'evil-generate-mode-line-tag :around #'bp-generate-mode-line-tag)
    (evil-mode +1)

    (bind-keys :map evil-insert-state-map
               ("C-x C-p" . evil-complete-previous-line)
               ("C-x C-n" . evil-complete-next-line))

    (bind-keys :map evil-normal-state-map
               :prefix "\\"
               :prefix-map evil-leader-prefix-map

               ;; Evil
               ("\\" . evil-ex-nohighlight)

               ;; Misc
               ("i"  . bp-open-iterm)
               (",i" . bp-find-init-file)
               ("bu" . browse-url)
               ("t"  . tldr)

               ;; Frames
               ("fn" . make-frame-command)
               ("fo" . other-frame)
               ("fc" . delete-frame)

               ;; Helm
               ("h" . helm-command-prefix)

               ;; Projectile
               ("p" . projectile-command-map)

               ;; Org
               ("oa"  . org-agenda)
               ("oc"  . helm-org-capture-templates)
               ("oh"  . helm-org-agenda-files-headings)
               ("ota" . bp-org-archive-task-at-point)

               ;; Notmuch
               ("mc" . compose-mail)
               ("mm" . notmuch)
               ("mt" . notmuch-tree)
               ("mi" . bp-notmuch-inbox)
               ("mu" . bp-notmuch-unread)

               ;; Winner
               ("wu" . winner-undo)
               ("wr" . winner-redo))

    (bind-key "SPC" evil-leader-prefix-map evil-normal-state-map)
    (bind-key "C-c C-\\" evil-leader-prefix-map)))

(use-package which-key
  :diminish which-key-mode
  :ensure t
  :config
  (which-key-mode +1))


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
                                  "#purescript" "#pixie-lang" "#elm" "#elixir-lang"))

   ;; Behave like a "normal" IRC client.
   erc-kill-buffer-on-part t
   erc-kill-queries-on-quit t
   erc-kill-server-buffer-on-quit t))

(use-package etags
  :init
  ;; Never append tags lists together.
  (setq tags-add-tables nil))

(use-package files
  :init
  (setq auto-save-file-name-transforms `((".*" ,(concat local-temp-dir "/\\1") t))
        backup-directory-alist         `((".*" . ,local-temp-dir))
        backup-by-copying t))

(use-package grep
  :commands (grep rgrep)
  :config
  (progn
    ;; Fish compatibility
    (grep-apply-setting
     'grep-find-command '("find . -type f -exec grep -nH -e  \\{\\} \\+" . 34))
    (grep-apply-setting
     'grep-find-template "find . <X> -type f <F> -exec grep <C> -inH -e <R> \\{\\} \\+")

    (use-package wgrep
      :ensure t)))

(use-package hl-line
  :config
  (progn
    (define-global-minor-mode bp-global-hl-line-mode global-hl-line-mode
      (lambda ()
        ;; XXX: You can't turn off global-hl-line-mode on a per-buffer
        ;; basis so we can just build up our own version that doesn't
        ;; activate for a given list of modes.
        (unless (memq major-mode (list 'eww-mode
                                       'term-mode
                                       'org-agenda-mode))
          (hl-line-mode +1))))

    (bp-global-hl-line-mode)))

(use-package swiper
  :disabled t
  :ensure t
  :bind (("C-s" . swiper)))

(use-package helm
  :diminish helm-mode
  :ensure t
  :bind (("C-;"     . helm-M-x)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x C-i" . helm-semantic-or-imenu))
  :config
  (progn
    (require 'helm-config)

    (use-package helm-ag
      :ensure t)

    (use-package helm-descbinds
      :ensure t
      :commands helm-descbinds)

    (use-package helm-flx
      :ensure t)

    ;; http://emacsist.com/10477
    (add-to-list 'display-buffer-alist
                 '("\\`\\*helm.*\\*\\'"
                   (display-buffer-in-side-window)
                   (inhibit-same-window . t)
                   (window-height . 0.4)))

    (setq helm-split-window-in-side-p t

          helm-ff-newfile-prompt-p nil
          helm-ff-skip-boring-files t

          helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-imenu-fuzzy-match t
          helm-recentf-fuzzy-match t
          helm-locate-fuzzy-match nil
          helm-semantic-fuzzy-match t)

    (helm-mode +1)
    (helm-autoresize-mode +1)
    (helm-flx-mode +1)

    (bind-keys :map helm-map
               ("C-w" . backward-kill-word))))

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
    ;; (ido-mode +1)
    ;; (ido-everywhere +1)

    (use-package ido-ubiquitous
      :disabled t
      :ensure t
      :init
      (ido-ubiquitous-mode +1))

    (use-package ido-vertical-mode
      :disabled t
      :ensure t
      :init
      (setq ido-vertical-show-count t)
      (ido-vertical-mode +1))

    (use-package ido-clever-match
      :disabled t
      :load-path "vendor/ido-clever-match"
      :config
      (ido-clever-match-enable))))

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

    (defun bp-term-sentinel (process event)
      (bp-term-kill))

    (defun bp-maybe-switch-to-buffer (buffer)
      "Switch to BUFFER iff it is non-nil."
      (when buffer
        (switch-to-buffer buffer)))

    (defun bp-term-add ()
      "Add a new terminal and jump to it."
      (interactive)
      (let ((default-directory (expand-file-name "~/")))
        (zipper-end bp-term-terms)
        (zipper-append bp-term-terms (ansi-term bp-term-shell))
        (bp-term-next)
        (set-process-sentinel (get-buffer-process bp-term-current-term-buffer) #'bp-term-sentinel)))

    (defun bp-term-kill ()
      "Kill the current terminal."
      (interactive)
      (cond
       ((>= (length (zipper-rhs bp-term-terms)) 1)
        (let ((buffer (zipper-drop bp-term-terms)))
          (kill-buffer bp-term-current-term-buffer)
          (setq bp-term-current-term-buffer buffer)
          (bp-maybe-switch-to-buffer buffer)))

       (t
        (zipper-drop bp-term-terms)
        (kill-buffer bp-term-current-term-buffer)
        (bp-term-prev)
        (unless bp-term-current-term-buffer
          (bp-term-toggle)))))

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
      (term-send-raw-string (or (evil-get-register ?+) "")))

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
  (winner-mode +1))


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
  :disabled t
  :bind ("C-x C-i" . imenu))

(use-package smex
  :disabled t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-;" . smex))
  :ensure t
  :init
  (setq smex-save-file (locate-user-emacs-file ".smex-items"))
  :config
  (smex-initialize))

(use-package semantic
  :commands semantic-mode
  :init
  (add-hook 'prog-mode-hook #'semantic-mode))


;;; UI
(use-package smart-mode-line
  :disabled t
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t
        sml/theme nil)
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
         ("MERGE_MSG\\'"      . git-commit-mode))
  :bind ("C-c m" . magit-status)
  :config
  (use-package fullframe
    :disabled t
    :ensure t
    :config
    (fullframe magit-status magit-mode-quit-window)))

(use-package git-timemachine
  :commands git-timemachine
  :ensure t)

(use-package diff-hl
  :disabled t
  :ensure t
  :config
  (global-diff-hl-mode +1))


;;; Org
(use-package org
  :ensure t
  :defer t
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
    (use-package ob-http
      :ensure t)

    (use-package ox-twbs
      :commands (org-twbs-export-as-html org-twbs-export-to-html)
      :ensure t)

    ;;; Misc
    ;; Paths to my org files
    (defvar bp-org-dir (expand-file-name "~/Dropbox/Documents/Personal"))
    (defvar bp-org-main-file (expand-file-name (concat bp-org-dir "/Bogdan.org")))


    ;;; Completion
    (setq org-outline-path-complete-in-steps nil)


    ;;; Code blocks
    ;; Highlight code in BEGIN_SRC-END_SRC blocks.
    (setq org-src-fontify-natively t)


    ;;; Babel
    ;; Allow these languages to be executed in org code blocks.
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((haskell . t)
       (http    . t)
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
    (add-hook 'org-mode-hook #'auto-fill-mode)))


;;; Code completion
(use-package company
  :diminish company-mode
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (bind-key "C-c C-y" #'company-yasnippet))

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
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(haskell-ghc
                                             html-tidy
                                             javascript-jshint
                                             json-jsonlint)))


;;; File navigation
(use-package projectile
  :diminish projectile-mode
  :ensure t
  :preface
  (defun bp-projectile-after-switch-hook ()
    (when (s-ends-with? ".py" (buffer-file-name))
      (let* ((path-segments (s-split "/" (projectile-project-root)))
             (path-segments (-remove #'string-empty-p path-segments))
             (project (car (-slice path-segments -1))))
        (bp-workon project))))
  :init
  (setq projectile-enable-caching t)
  :config
  (progn
    (use-package helm-projectile
      :ensure t
      :config
      (helm-projectile-on))

    (add-hook 'projectile-after-switch-project-hook #'bp-projectile-after-switch-hook)

    (projectile-global-mode)))


;;; Process management
(use-package prodigy
  :bind (("C-c P" . prodigy))
  :ensure t
  :config
  (use-package bp-prodigy-services))


;;; Miscellaneous
(use-package s
  :ensure t)

(use-package time
  :config
  (progn
    (setq display-time-default-load-average nil)

    (display-time-mode +1)))

(use-package diminish
  :commands diminish
  :ensure t)

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :commands exec-path-from-shell-initialize
    :ensure t
    :init
    (add-hook 'after-init-hook #'exec-path-from-shell-initialize)))

(use-package paradox
  :commands paradox-list-packages
  :ensure t
  :init
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(use-package tldr
  :commands tldr
  :ensure t)


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
      :config
      (progn
        (use-package company-irony
          :ensure t
          :preface
          (defun bp-company-irony-setup-hook ()
            (add-to-list 'company-backends 'company-irony))
          :init
          (progn
            (add-hook 'irony-mode-hook #'bp-company-irony-setup-hook)
            (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)))

        (use-package flycheck-irony
          :ensure t
          :preface
          (defun bp-flycheck-irony-setup-hook ()
            (add-to-list 'flycheck-checkers 'irony))
          :init
          (add-hook 'irony-mode-hook #'bp-flycheck-irony-setup-hook))

        (use-package irony-eldoc
          :commands irony-eldoc
          :ensure t)

        (add-hook 'irony-mode-hook #'eldoc-mode)
        (add-hook 'irony-mode-hook #'irony-eldoc)))

    (add-hook 'c-mode-hook #'bp-c-mode-hook)
    (add-hook 'c-mode-hook #'irony-mode)))


;;; Clojure
(use-package clojure-mode
  :ensure t
  :mode (("\\.cljs?\\'" . clojure-mode)
         ("\\.boot\\'"  . clojure-mode))
  :config
  (progn
    (use-package cider
      :ensure t
      :config
      (progn
        (add-hook 'cider-repl-mode-hook #'my-cider-mode-hook)
        (add-hook 'cider-repl-mode-hook #'paredit-mode)
        (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
        (add-hook 'cider-mode-hook #'my-cider-mode-hook)
        (add-hook 'cider-mode-hook #'eldoc-mode)
        (add-hook 'cider-mode-hook #'paredit-mode)
        (add-hook 'cider-mode-hook #'rainbow-delimiters-mode)

        (bind-keys :map cider-mode-map
                   ("C-c ." . cider-jump-to-var)
                   ("C-c ," . cider-pop-back))))

    (add-hook 'clojure-mode-hook #'cider-mode)))


;;; Common Lisp
(use-package slime
  :commands slime
  :ensure t
  :init
  (add-hook 'slime-mode-hook #'paredit-mode)
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))


;;; Docker
(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
  :ensure t)


;;; Erlang and Elixir
(use-package erlang
  :mode ("\\.erl\\'" . erlang-mode)
  :ensure t)

(use-package alchemist
  :mode ("\\.exs?\\'" . elixir-mode)
  :ensure t
  :init
  (add-hook 'elixir-mode-hook #'alchemist-mode)
  (add-hook 'elixir-mode-hook #'yas-minor-mode)
  :config
  (progn
    (setq alchemist-goto-erlang-source-dir (expand-file-name "~/sandbox/erlang-otp")
          alchemist-goto-elixir-source-dir (expand-file-name "~/sandbox/elixir"))

    (bind-keys :map elixir-mode-map
               ("C-c ." . alchemist-goto-definition-at-point))))


;;; Elm
(use-package elm-mode
  :load-path "vendor/elm-mode"
  :mode ("\\.elm\\'" . elm-mode)
  :config
  (progn
    (setq elm-tags-on-save t
          elm-tags-exclude-elm-stuff nil)

    (add-to-list 'company-backends 'company-elm)))


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

(use-package smartparens
  :diminish smartparens-mode
  :ensure t
  :commands smartparens-mode
  :init
  (add-hook 'elixir-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'irony-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'perl6-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'python-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'scala-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'org-mode-hook #'turn-on-smartparens-strict-mode)
  :config
  (progn
    (require 'smartparens-config)

    (sp-with-modes
     'org-mode
     (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
     (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
     (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
     (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC"))))

    (sp-with-modes
     'scala-mode
     (sp-local-pair "(" nil :post-handlers '(("||\n[i]" "RET")))
     (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))

    (bind-keys :map smartparens-mode-map
               ("C-M-a" . sp-beginning-of-sexp)
               ("C-M-e" . sp-end-of-sexp)
               ("C-M-u" . sp-backward-up-sexp)
               ("C-M-d" . sp-down-sexp)
               ("C-M-b" . sp-backward-sexp)
               ("C-M-f" . sp-forward-sexp)
               ("C-M-n" . sp-next-sexp)
               ("C-M-p" . sp-previous-sexp)
               ("C-M-k" . sp-kill-sexp)
               ("C-M-t" . sp-transpose-sexp)
               ("C-("   . sp-backward-slurp-sexp)
               ("C-)"   . sp-forward-slurp-sexp)
               ("C-{"   . sp-backward-barf-sexp)
               ("C-}"   . sp-forward-barf-sexp))))


;;; Fish
(use-package fish-mode
  :ensure t
  :mode "\\.fish\\'")


;;; Groovy
(use-package groovy-mode
  :mode (("\\.gradle\\'" . groovy-mode)
         ("\\.groovy\\'" . groovy-mode))
  :ensure t)


;;; Haskell
(use-package haskell-mode
  :mode "\\.l?hs\\'"
  :ensure t
  :config
  (progn
    (require 'haskell-interactive-mode)
    (require 'haskell-process)

    (use-package company-ghc
      :disabled t
      :ensure t
      :config
      (add-to-list 'company-backends 'company-ghc))

    (use-package company-ghci
      :ensure t
      :config
      (add-to-list 'company-backends 'company-ghci))

    (use-package flycheck-haskell
      :disabled t
      :ensure t
      :config
      (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

    (use-package shm
      :disabled t
      :ensure t
      :init
      (add-hook 'haskell-mode-hook #'structured-haskell-mode)
      :config
      (custom-set-variables
       '(shm-auto-insert-skeletons t)
       '(shm-use-presentation-mode t)))

    (use-package shakespeare-mode
      :ensure t)

    (custom-set-variables
     ;; Haskell Process
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
    (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
    (add-hook 'haskell-mode-hook #'interactive-haskell-mode)

    (bind-keys :map haskell-mode-map
               ("C-j" . haskell-indentation-newline-and-indent)
               ("C-c ." . haskell-mode-jump-to-def-or-tag)
               ("C-c M-l" . haskell-process-reload-devel-main))))


;;; Javascript
(use-package js2-mode
  :mode ("\.jsx?\\'" . js2-mode)
  :ensure t
  :config
  (setq js2-basic-offset 2
        js2-strict-missing-semi-warning t))

(use-package ember-mode
  :commands (ember-mode)
  :ensure t)

;;; JSON
(use-package json-mode
  :mode "\.json\\'"
  :ensure t)


;;; LaTeX
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . LaTeX-mode))


;;; LESS
(use-package less-css-mode
  :mode "\\.less\\'"
  :ensure t
  :config
  (progn
    (defun bp-scss-mode-hook ()
      (setq-local css-indent-offset 2))

    (add-hook 'less-css-mode-hook #'bp-scss-mode-hook)))


;;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
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
      :load-path "~/.opam/system/share/emacs/site-lisp"
      :commands (merlin-mode)
      :init
      (setq merlin-error-after-save t
            merlin-command 'opam))

    (use-package ocp-indent
      :load-path "~/.opam/system/share/emacs/site-lisp"
      :config
      (setq ocp-indent-syntax '("lwt")))

    (use-package flycheck-ocaml
      :load-path "vendor/flycheck-ocaml"
      :commands (flycheck-ocaml-setup))

    (add-hook 'tuareg-mode-hook #'merlin-mode)
    (add-hook 'tuareg-mode-hook #'flycheck-ocaml-setup)
    (add-hook 'tuareg-mode-hook #'utop-minor-mode)))


;;; Perl 6
(use-package perl6-mode
  :mode (("\\.p6\\'"  . perl6-mode)
         ("\\.pm6\\'" . perl6-mode))
  :ensure t
  :config
  (progn
    (use-package flycheck-perl6
      :ensure t)))


;;; Python
(use-package pyvenv
  :ensure t)

(use-package python
  :mode (("\\.py\\'"   . python-mode)
         ("SConstruct" . python-mode))
  :interpreter ("python" . python-mode)
  :preface
  (eval-when-compile
    (declare-function py-test-define-project "py-test"))

  (defun bp-apply-buffer-env (buffer-name)
    (with-current-buffer buffer-name
      (goto-char (point-min))
      (dolist (binding (json-read))
        (let ((env (format "%s=%s" (car binding) (cdr binding))))
          (when (not (member env process-environment))
            (setq process-environment (cons env process-environment))))
        (when (eq (car binding) 'PATH)
          (setq exec-path (split-string (cdr binding) ":"))))))

  (defun bp-workon (name)
    (interactive
     (list (completing-read "Work on: " (pyvenv-virtualenv-list)
                            nil t nil 'pyvenv-workon-history nil nil)))
    (let ((output-buffer "*vf-activate*")
          (error-buffer "*vf-activate-errors*"))
      (when (buffer-live-p output-buffer)
        (with-current-buffer output-buffer
          (erase-buffer)))
      (shell-command
       (format "vf activate %s; and python -c 'import json, os; print(json.dumps(dict(os.environ)))'" name)
       output-buffer
       error-buffer)
      (bp-apply-buffer-env output-buffer)
      (message (concat "Activated virtualenv " name))))
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
                   ("C-c v" . bp-workon)
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
          "\\r" 'py-test-run-test-at-point
          "\\T" 'py-test-run-directory
          "\\t" 'py-test-run-file)

        ;; Purty mode-line.
        (setq py-test-*mode-line-face-shenanigans-on* t)
        (setq py-test-*mode-line-face-shenanigans-timer* "0.5 sec")

        (use-package bp-py-test-projects)))))

(use-package coverage-mode
  :commands coverage-mode
  :load-path "vendor/coverage-mode")


;;; Purescript
(use-package purescript-mode
  :ensure t
  :mode "\\.purs\\'"
  :config
  (progn
    (add-hook 'purescript-mode-hook #'turn-on-purescript-indentation)

    (bind-keys :map purescript-mode-map
               ("C-j" . purescript-newline-and-indent))))


;;; REST
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :ensure t
  :config
  (use-package company-restclient
    :ensure t
    :config
    (add-to-list 'company-backends 'company-restclient)))


;;; Rust
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :ensure t
  :config
  (progn
    (use-package racer
      :ensure t
      :init
      (setq racer-rust-src-path (expand-file-name "~/sandbox/rust/src")))

    (add-hook 'rust-mode-hook #'eldoc-mode)
    (add-hook 'rust-mode-hook #'racer-mode)))


;;; Scala
(use-package sbt-mode
  :commands sbt-start
  :ensure t)

(use-package scala-mode2
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'"   . scala-mode))
  :ensure t)

(use-package ensime
  :commands ensime-scala-mode-hook
  :ensure t
  :preface
  (defun bp-scala-mode-hook ()
    (when (string-suffix-p ".sbt" (buffer-name))
      (flycheck-mode -1)))
  :init
  (progn
    (add-hook 'scala-mode-hook #'ensime-scala-mode-hook)
    (add-hook 'scala-mode-hook #'bp-scala-mode-hook))
  :config
  (progn
    (setq ensime-auto-generate-config t
          ensime-default-java-flags '("-Xms512M" "-Xmx1G")
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
  :disabled t
  :ensure t
  :init
  (progn
    (add-hook 'geiser-mode-hook #'paredit-mode)
    (add-hook 'geiser-mode-hook #'rainbow-delimiters-mode)
    (setq geiser-active-implementations '(chicken))))


;;; SCSS
(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :config
  (progn
    ;; Stupid functionality is stupid.
    (setq scss-compile-at-save nil)
    (setq-default css-indent-offset 2)))


;;; Swift
(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'"
  :config
  (add-to-list 'flycheck-checkers 'swift))


;;; Terraform
(use-package terraform-mode
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
         ("\\.eex\\'"   . web-mode))
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

        web-mode-engines-alist '(("django" . "\\.html\\'")
                                 ("razor"  . "\\.scala\\.html\\'")
                                 ("elixir" . "\\.eex\\'"))))


;;; Yaml
(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")


;;; Message
(use-package message
  :init
  (setq message-auto-save-directory (expand-file-name "~/Maildir/drafts")))


;;; Notmuch
(use-package notmuch
  :ensure t
  :commands (notmuch notmuch-search notmuch-tree)
  :preface
  (defun bp-notmuch-force-sync ()
    (interactive)
    (start-process "notmuch-sync" "*notmuch-sync*" "notmuch-sync"))

  (defun bp-notmuch-inbox ()
    (interactive)
    (notmuch-search "tag:inbox"))

  (defun bp-notmuch-unread ()
    (interactive)
    (notmuch-search "tag:unread"))

  (defun bp-notmuch-archive (&optional beg end)
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag '("-unread" "-inbox") beg end)
    (notmuch-search-next-thread))

  (defun bp-notmuch-spam (&optional beg end)
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag '("+spam" "-unread" "-inbox") beg end)
    (notmuch-search-next-thread))

  (defun bp-notmuch-todo (&optional beg end)
    (interactive (notmuch-search-interactive-region))
    (if (member "todo" (notmuch-search-get-tags))
        (notmuch-search-tag '("-todo") beg end)
      (notmuch-search-tag '("+todo" "-unread" "-inbox")))
    (notmuch-search-next-thread))

  (defun bp-notmuch-trash (&optional beg end)
    (interactive (notmuch-search-interactive-region))
    (if (member "trash" (notmuch-search-get-tags))
        (notmuch-search-tag '("-trash") beg end)
      (notmuch-search-tag '("+trash" "-unread" "-inbox")))
    (notmuch-search-next-thread))

  (defun bp-after-select-identity (&rest r)
    (let* ((identity (assoc gnus-alias-current-identity gnus-alias-identity-alist))
           (address (nth 2 identity))
           (address (cadr (mail-extract-address-components address))))
      (setq smtpmail-smtp-user address)))

  (defun mimedown ()
    (interactive)
    (save-excursion
      (message-goto-body)
      (shell-command-on-region (point) (point-max) (concat "mimedown " gnus-alias-current-identity) nil t)))
  :config
  (progn
    (use-package gnus-art)
    (use-package gnus-alias
      :ensure t
      :config
      (progn
        (advice-add #'gnus-alias-select-identity :after #'bp-after-select-identity)

        (setq gnus-alias-identity-alist
              '(("personal" nil "Bogdan Popa <popa.bogdanp@gmail.com>" nil nil nil nil)
                ("cleartype" nil "Bogdan Popa <bogdan@cleartype.io>" "CLEARTYPE SRL" nil nil nil)
                ("defn" nil "Bogdan Popa <bogdan@defn.io>" "CLEARTYPE SRL" nil nil nil)
                ("work" nil "Bogdan Popa <bogdan@ave81.com>" "LeadPages" nil nil nil))

              gnus-alias-default-identity "personal")

        (add-hook 'message-setup-hook #'gnus-alias-init)))

    (setq notmuch-search-oldest-first nil

          notmuch-saved-searches
          '((:name "inbox"    :query "tag:inbox"   :key "i")
            (:name "unread"   :query "tag:unread"  :key "u")
            (:name "flagged"  :query "tag:flagged" :key "f")
            (:name "todo"     :query "tag:todo"    :key "t")
            (:name "sent"     :query "tag:sent"    :key "s")
            (:name "drafts"   :query "tag:draft"   :key "d")
            (:name "all mail" :query "*"           :key "a"))

          notmuch-hello-sections
          '(notmuch-hello-insert-search
            notmuch-hello-insert-recent-searches
            notmuch-hello-insert-saved-searches
            notmuch-hello-insert-alltags))

    (bind-keys :map notmuch-search-mode-map
               ("A" . bp-notmuch-archive)
               ("S" . bp-notmuch-spam)
               ("T" . bp-notmuch-todo)
               ("d" . bp-notmuch-trash))))


(provide 'init)
;;; init.el ends here
