;;; init.el --- main config entry point -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;;; Misc. builtin options
(setq
 ;;; Me
 user-full-name "Bogdan Popa"
 user-mail-address "popa.bogdanp@gmail.com"

 ;;; GC
 ;; EMACS' default GC threshold is <1MB. Give it 16MB instead.
 gc-cons-threshold 16777216
 garbage-collection-messages nil

 ;;; auto-compile
 load-prefer-newer t

 ;;; Don't warn on redefinition
 ad-redefinition-action 'accept

 ;;; Don't attempt to load `default.el'
 inhibit-default-init t

 ;; Disable welcome screen.
 inhibit-startup-message t

 ;;; Mac port
 mac-option-modifier 'meta
 mac-command-modifier 'hyper
 mac-mouse-wheel-smooth-scroll nil)

(set-language-environment "utf-8")

(setq-default
 ;;; Custom
 ;; Ensure custom values are saved to an ignored file.
 custom-file (locate-user-emacs-file "custom.el")

 ;;; Editing
 ;; Never use tabs.
 indent-tabs-mode nil

 ;; When using tabs (Go), they should be 4 spaces long.
 tab-width 2

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
 ;; No bell of any kind.
 ring-bell-function (lambda ())
 visible-bell nil

 ;; Make scrolling behave like it does in VIM.
 scroll-margin 3
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
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize nil)

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

  (require 'server)
  (unless (server-running-p)
    (server-start))

  (use-package better-default-theme
    :disabled t
    :load-path "vendor/better-default-theme"
    :config (load-theme 'better-default t))

  (use-package twilight-bright-theme
    :load-path "vendor/twilight-bright-theme"
    :config (load-theme 'twilight-bright t)))


;;; EVIL
(use-package evil
  :load-path "vendor/evil"
  :pin manual
  :preface
  (defvar bp-evil-modes
    '(fundamental-mode conf-mode css-mode evil-command-window-mode
                       groovy-mode haskell-mode haskell-cabal-mode json-mode
                       prog-mode purescript-mode restclient-mode rust-mode
                       text-mode sass-mode tuareg-mode web-mode yaml-mode)
    "The list of modes that should default to normal mode.  All modes
    derived from these will also default to evil normal mode.")

  (defun bp-apply-evil-mode-hook ()
    (if (apply #'derived-mode-p bp-evil-modes)
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
       ((string= state "emacs")
        (propertize tag 'face '((:background "red" :foreground "white"))))
       (t tag))))

  (defun bp-find-init-file ()
    (interactive)
    (find-file (locate-user-emacs-file "init.el")))

  (defun bp--open-app (name)
    (do-applescript (format "tell application \"%s\" to activate" name)))

  (defun bp-open-iterm ()
    (interactive)
    (bp--open-app "iTerm"))

  (defun bp-open-chrome ()
    (interactive)
    (bp--open-app "Google Chrome"))
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
      :load-path "vendor/evil-surround"
      :config
      (add-hook 'evil-mode-hook #'global-evil-surround-mode))

    (dolist (hook '(git-commit-setup-hook
                    git-timemachine-mode-hook
                    magit-blame-mode-hook
                    org-capture-mode-hook
                    org-log-buffer-setup-hook))
      (add-hook hook #'bp-toggle-emacs-state))

    (add-to-list 'evil-emacs-state-modes 'notmuch-tree-mode)
    (add-to-list 'evil-emacs-state-modes 'alchemist-help-minor-mode)

    (add-hook 'minibuffer-setup-hook #'bp-minibuffer-setup-hook)
    (add-hook 'after-change-major-mode-hook #'bp-apply-evil-mode-hook)
    (advice-add #'evil-generate-mode-line-tag :around #'bp-generate-mode-line-tag)
    (evil-mode +1)

    (bind-keys ("C-c C-\\" . evil-leader-prefix-map)
               ("C-c M-a"  . bp-open-iterm)
               ("C-c M-c"  . bp-open-chrome)
               ("C-j"      . newline-and-indent)
               ("C-w"      . backward-kill-word)
               ("C--"      . text-scale-decrease)
               ("C-="      . text-scale-increase)
               ("C-+"      . text-scale-increase)
               ("H-f"      . bp-toggle-fullscreen))

    (bind-keys :map evil-insert-state-map
               ("C-x C-p" . evil-complete-previous-line)
               ("C-x C-n" . evil-complete-next-line))

    (bind-keys :map evil-normal-state-map
               :prefix "SPC"
               :prefix-map evil-leader-prefix-map
               ("SPC" . recompile)
               ("\\"  . evil-ex-nohighlight)
               ("i"   . bp-open-iterm)
               (",i"  . bp-find-init-file)
               ("bu"  . browse-url)
               ("fn"  . make-frame-command)
               ("fo"  . other-frame)
               ("fc"  . delete-frame)
               ("p"   . projectile-command-map)
               ("oa"  . org-agenda)
               ("oc"  . org-capture)
               ("mc"  . compose-mail)
               ("mm"  . notmuch)
               ("mt"  . notmuch-tree)
               ("mi"  . bp-notmuch-inbox)
               ("mu"  . bp-notmuch-unread)
               ("xf"  . xref-find-definitions))))


;;; Builtins
(use-package autorevert
  :config
  (global-auto-revert-mode))

(use-package compile
  :preface
  (defun bp-compilation-mode-hook ()
    (setq-local scroll-margin 0))

  (defun bp-make-at (root-path)
    (interactive "fFilename: ")

    (let ((default-directory (locate-dominating-file buffer-file-name (f-filename root-path))))
      (compile "make")))

  (defun bp-make-elm ()
    (interactive)
    (bp-make-at "elm-package.json"))

  (defun bp-make ()
    (interactive)
    (bp-make-at "Makefile"))
  :init
  (setq compilation-scroll-output t)
  (add-hook 'compilation-mode-hook #'bp-compilation-mode-hook))

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

(use-package etags
  :init
  (setq tags-add-tables nil
        tags-revert-without-query t))

(use-package files
  :init
  (setq auto-save-file-name-transforms `((".*" ,(concat local-temp-dir "/\\1") t))
        backup-directory-alist         `((".*" . ,local-temp-dir))
        backup-by-copying t))

(use-package ag
  :commands (ag)
  :ensure t
  :init
  (setq ag-highlight-search t
        ag-reuse-buffers t))

(use-package grep
  :commands (grep rgrep)
  :config
  (progn
    ;; Fish compatibility
    (grep-apply-setting
     'grep-find-command '("find . -type f -exec grep -nH -e  \\{\\} \\+" . 34))
    (grep-apply-setting
     'grep-find-template "find . <X> -type f <F> -exec grep <C> -inH -e <R> \\{\\} \\+")))

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

(use-package ido
  :bind (("C-x C-i" . imenu))
  :init
  (progn
    (setq ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-use-virtual-buffers t
          ido-handle-duplicate-virtual-buffers 2
          ido-max-prospects 10
          ido-ignore-extensions t
          ido-auto-merge-work-directories-length -1)

    (add-hook 'after-init-hook #'ido-mode))
  :config
  (progn
    (use-package ido-ubiquitous :ensure t)

    (ido-everywhere)
    (ido-ubiquitous)))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-;" . smex)))

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
        recentf-max-saved-items 100
        recentf-max-menu-items 10
        recentf-auto-cleanup 60)
  :config
  (progn
    (add-to-list 'recentf-exclude "/.virtualenvs/")
    (add-to-list 'recentf-exclude "/elpa/")
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
    (add-to-list 'recentf-exclude "MERGE_MSG\\'")
    (add-to-list 'recentf-exclude "TAGS\\'")
    (add-to-list 'recentf-exclude ".el.gz")

    (recentf-mode +1)))

(use-package savehist
  :init
  (setq savehist-file (locate-user-emacs-file "savehist")
        savehist-additional-variables '(search ring regexp-search-ring))
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

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))


;;; Buffers and buffer navigation
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :preface
  (eval-when-compile
    (declare-function ibuffer-do-sort-by-alphabetic "ibuf-ext"))

  (defun bp-ibuffer-hook ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :config
  (progn
    (use-package ibuffer-vc
      :commands ibuffer-vc-set-filter-groups-by-vc-root
      :ensure t)

    (add-hook 'ibuffer-hook #'bp-ibuffer-hook)))


;;; Git
(use-package magit
  :ensure t
  :commands (magit-status git-commit-mode)
  :mode (("COMMIT_EDITMSG\\'" . git-commit-mode)
         ("MERGE_MSG\\'"      . git-commit-mode))
  :bind ("C-c m" . magit-status)
  :init
  (setq magit-completing-read-function #'magit-ido-completing-read)
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
  :ensure t
  :commands (org-agenda org-capture)
  :mode ("\\.org\\'" . org-mode)
  :preface
  (defvar bp-org-dir (expand-file-name "~/Dropbox/Documents/Personal"))
  (defvar bp-org-main-file (expand-file-name (concat bp-org-dir "/Bogdan.org")))
  (defvar bp-org-agenda-files-path bp-org-dir)
  :config
  (progn
    (use-package bp-org-capture-templates)
    (use-package bp-org-agenda-commands)

    ;;; Babel
    ;; Allow these languages to be executed in org code blocks.
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python  . t)
       (sh      . t)
       (dot     . t)))

    (setq
     ;;; Completion
     org-outline-path-complete-in-steps nil

     ;;; Code blocks
     ;; Highlight code in BEGIN_SRC-END_SRC blocks.
     org-src-fontify-natively t

     ;;; Capture
     ;; Where to put captured stuff.
     org-default-notes-file bp-org-main-file

     ;;; TODOs
     ;; Log the closing time of TODO items.
     org-log-done 'time

     ;; Better todo states.
     org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))

     ;; Refile anywhere.
     org-refile-targets '((nil :maxlevel . 9))

     ;;; Agenda
     ;; Set up path to agenda files.
     org-agenda-files (list bp-org-agenda-files-path)

     ;; Don't ruin my window setup, org-agenda.
     org-agenda-window-setup 'current-window)

    ;;; Text editing
    (add-hook 'org-mode-hook #'auto-fill-mode)))


;;; Code completion
(use-package company
  :diminish company-mode
  :ensure t
  :init
  (progn
    (setq company-idle-delay 0.25)

    (add-hook 'after-init-hook #'global-company-mode))
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
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(sass))
  (setq-default flycheck-flake8rc "setup.cfg"))


;;; File navigation
(use-package projectile
  :diminish projectile-mode
  :ensure t
  :preface
  (defun bp-projectile-find-file-hook ()
    (let ((name (projectile-project-name)))
      (when (and (not (equal name bp-current-python-env))
                 (-contains-p (pyvenv-virtualenv-list) name))
          (bp-workon name))))
  :init
  (setq projectile-enable-caching t)
  (add-hook 'after-init-hook #'projectile-global-mode)
  (add-hook 'projectile-find-file-hook #'bp-projectile-find-file-hook))


;;; Miscellaneous
(use-package s
  :ensure t)

(use-package time
  :init
  (setq display-time-default-load-average nil)
  (add-hook 'after-init-hook #'display-time-mode))

(use-package diminish
  :commands diminish
  :ensure t)

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :commands exec-path-from-shell-initialize
    :ensure t
    :init
    (add-hook 'after-init-hook #'exec-path-from-shell-initialize)
    :config
    (add-to-list 'exec-path-from-shell-variables "GOPATH")))


;;; C
(use-package cc-mode
  :mode ("\\.c\\'" . c-mode)
  :init
  (setq c-default-style "bsd"
        c-basic-offset 4))


;;; Cedar
(use-package cedar-mode
  :load-path "~/sandbox/cedar-mode"
  :mode "\\.cedar\\'")


;;; Clojure
(use-package cider
  :ensure t
  :config
  (progn
    (define-clojure-indent
      (defroutes 'defun) (context 2)
      (ANY 2) (DELETE 2) (HEAD 2) (GET 2) (POST 2) (PUT 2))))


;;; Docker
(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
  :ensure t)


;;; Elm
(use-package elm-mode
  :load-path "vendor/elm-mode"
  :mode ("\\.elm\\'" . elm-mode)
  :init
  (progn
    (setq elm-indent-offset 4
          elm-format-on-save t
          elm-sort-imports-on-save t
          elm-tags-on-save t)
    (add-to-list 'company-backends #'company-elm)
    (add-hook 'elm-mode-hook #'eldoc-mode)))


;;; Emacs lisp
(use-package eldoc
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package paredit
  :diminish paredit-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode))

(use-package smartparens
  :diminish smartparens-mode
  :ensure t
  :commands smartparens-mode
  :init
  (add-hook 'irony-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'python-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'scala-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'org-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'go-mode-hook #'turn-on-smartparens-strict-mode)
  :config
  (progn
    (require 'smartparens-config)
    (require 'smartparens-scala)

    (sp-with-modes
        'org-mode
      (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
      (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC"))))

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


;;; Go
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :preface
  (defun bp-go-mode-hook ()
    ;; Fixes fill-region over comments: https://github.com/dominikh/go-mode.el/issues/119
    (set (make-local-variable 'adaptive-fill-regexp) "[   ]*\\(//+\\|\\**\\)[     ]*\\([  ]*\\([-–!|#%;>*·•‣⁃◦]+[  ]*\\)*\\)")
    (set (make-local-variable 'compile-command) "go install -v; and go test -v; and go vet"))
  :init
  (setq gofmt-command "goimports")
  :config
  (progn
    (use-package company-go
      :ensure t
      :config
      (add-to-list 'company-backends #'company-go))

    (use-package go-eldoc
      :ensure t)

    (load (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/guru/go-guru.el"))

    (add-hook 'go-mode-hook #'bp-go-mode-hook)
    (add-hook 'go-mode-hook #'go-eldoc-setup)
    (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
    (add-hook 'go-mode-hook #'yas-minor-mode)
    (add-hook 'before-save-hook #'gofmt-before-save)

    (bind-keys :map go-mode-map
               ("C-c ." . godef-jump))))


;;; Guile
(use-package geiser
  :commands run-geiser
  :ensure t)


;;; Groovy
(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'")


;;; Haskell
(use-package intero
  :load-path "vendor/intero/elisp"
  :init
  (setq haskell-stylish-on-save t)
  (add-hook 'haskell-mode-hook #'intero-mode))

(use-package hindent
  :disabled t
  :ensure t
  :init
  (setq hindent-reformat-buffer-on-save t)
  (add-hook 'haskell-mode-hook #'hindent-mode))


;;; Javascript
(use-package js2-mode
  :mode ("\.jsx?\\'" . js2-mode)
  :ensure t
  :config
  (setq js2-basic-offset 2
        js2-strict-missing-semi-warning t
        js2-global-externs '("module" "require" "describe" "it" "process" "__dirname")))


;;; JSON
(use-package json-mode
  :mode "\.json\\'"
  :ensure t
  :config
  (setq json-reformat:indent-width 2
        js-indent-level 2))


;;; SASS
(use-package sass-mode
  :mode (("\\.sass\\'" . sass-mode)
         ("\\.scss\\'" . scss-mode))
  :ensure t
  :init
  (setq css-indent-offset 2))


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
      :ensure t
      :commands (flycheck-ocaml-setup))

    (add-hook 'tuareg-mode-hook #'merlin-mode)
    (add-hook 'tuareg-mode-hook #'flycheck-ocaml-setup)
    (add-hook 'tuareg-mode-hook #'utop-minor-mode)))


;;; Python
(use-package pyvenv :ensure t)
(use-package python
  :mode (("\\.py\\'"   . python-mode)
         ("SConstruct" . python-mode))
  :interpreter ("python" . python-mode)
  :preface
  (defvar bp-current-python-env nil)

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
      (setq bp-current-python-env name)
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
             elpy-module-sane-defaults))))))

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

        (use-package bp-py-test-projects)))

    (add-hook 'python-mode-hook #'yas-minor-mode)))


;;; Purescript
(use-package purescript-mode
  :ensure t
  :config
  (progn
    (use-package psc-ide
      :load-path "vendor/psc-ide")

    (add-hook 'purescript-mode-hook #'psc-ide-mode)
    (add-hook 'purescript-mode-hook #'turn-on-purescript-indentation)))


;;; REST
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :ensure t)


;;; Rust
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :ensure t
  :config
  (progn
    (use-package cargo :ensure t)
    (use-package racer :ensure t)
    (use-package flycheck-rust :ensure t)

    (setq racer-rust-src-path (expand-file-name "~/sandbox/rust/src/")
          rust-format-on-save t)

    (add-hook 'rust-mode-hook #'cargo-minor-mode)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'rust-mode-hook #'flycheck-rust-setup)
    (add-hook 'racer-mode-hook #'eldoc-mode)))


;;; Scala
(use-package sbt-mode
  :commands sbt-start sbt-command
  :ensure t)

(use-package scala-mode
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'"   . scala-mode))
  :interpreter ("scala" . scala-mode)
  :ensure t)

(use-package ensime
  :pin melpa-stable
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


;;; TOML
(use-package toml-mode
  :mode ("\\.toml\\'" . toml-mode)
  :ensure t)


;;; Web
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.mjml\\'"  . web-mode)
         ("\\.vue\\'"   . web-mode)
         ("\\.hbs\\'"   . web-mode)
         ("\\.eex\\'"   . web-mode)
         ("\\.tmpl\\'"  . web-mode)
         ("\\.blade\\.php\\'" . web-mode))
  :init
  (progn
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
                                   ("blade"  . "\\.blade\\.")))))


;;; Yaml
(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")


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
  :init
  (setq message-auto-save-directory (expand-file-name "~/Maildir/drafts"))
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
            notmuch-hello-insert-alltags)

          notmuch-show-all-tags-list t)

    (bind-keys :map notmuch-search-mode-map
               ("A" . bp-notmuch-archive)
               ("S" . bp-notmuch-spam)
               ("T" . bp-notmuch-todo)
               ("d" . bp-notmuch-trash))))

(provide 'init)
;;; init.el ends here
