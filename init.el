;;; init.el --- main config entry point -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;;; Misc. builtin options
(setq
 ;; Me
 user-full-name "Bogdan Popa"
 user-mail-address "bogdan@defn.io"

 ;; Don't attempt to load packages.
 package-enable-at-startup nil

 ;; Ensure custom values are saved to an ignored file.
 custom-file (locate-user-emacs-file "custom.el")

 ;; EMACS' default GC threshold is <1MB. Give it 32MB instead.
 gc-cons-threshold (* 32 1024 1024)
 gc-cons-percentage 0.9
 garbage-collection-messages nil

 ;; Prefer source over bytecode if bytecode is outdated.
 load-prefer-newer t

 ;; Don't warn on redefinition
 ad-redefinition-action 'accept

 ;; Don't attempt to load `default.el'
 inhibit-default-init t

 ;; Disable welcome screen.
 inhibit-startup-message t

 ;; Mac port
 mac-option-modifier 'meta
 mac-command-modifier 'hyper
 mac-mouse-wheel-smooth-scroll nil)

(setq-default
 ;; Never use tabs.
 indent-tabs-mode nil

 ;; Mark empty lines.
 indicate-empty-lines t

 ;; When using tabs (Go), they should be 2 spaces long.
 tab-width 2

 ;; Don't wrap long lines.
 truncate-lines t)

;; Enable narrow to region functionality
(put 'narrow-to-region 'disabled nil)

;; Set up use-package.
(eval-when-compile
  (add-to-list 'load-path (locate-user-emacs-file "vendor/use-package"))
  (require 'cl)
  (require 'use-package))

;; Load packages that everything else depends on.
(use-package a        :load-path "vendor/a"        :defer t)
(use-package alert    :load-path "vendor/alert"    :defer t)
(use-package compat   :load-path "vendor/compat"   :defer t)
(use-package dash     :load-path "vendor/dash"     :defer t)
(use-package diminish :load-path "vendor/diminish" :commands (diminish))
(use-package f        :load-path "vendor/f"        :defer t :after (dash s))
(use-package ht       :load-path "vendor/ht"       :defer t)
(use-package popup    :load-path "vendor/popup"    :defer t)
(use-package s        :load-path "vendor/s"        :defer t)
(use-package smtpmail                              :defer t)
(use-package subr-x                                :defer t)


;; GC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bp-gc-after-init-hook ()
  "Lower GC params to avoid freezes."
  (interactive)
  (setq gc-cons-threshold (* 16 1024 1024)
        gc-cons-percentage 0.2))

(add-hook 'after-init-hook #'bp-gc-after-init-hook)


;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bp-make-font (size)
  "Make a font of the given SIZE."
  (format "-*-Dank Mono-medium-normal-normal-*-%d-*-*-*-m-0-iso10646-1" size))

;; Position and resize frame.
(add-to-list 'default-frame-alist `(font . ,(bp-make-font 14)))

;; Remove GUI elements.  Menu bar not removed because it makes the
;; emacs-mac port ignore Spaces.  Not a problem on macOS, but
;; potentially an issue on other platforms.
(dolist (mode '(blink-cursor-mode
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

(defvar bp-presentation-mode nil
  "Keeps track of whether or not we're in presentation mode.")

(defun bp-toggle-presentation-mode ()
  "Toggle the frame font between presentation and non-presentation mode."
  (interactive)
  (cond
   (bp-presentation-mode
    (set-frame-font (bp-make-font 14))
    (setq bp-presentation-mode nil))

   (t
    (set-frame-font (bp-make-font 18))
    (setq bp-presentation-mode t))))

(defun bp-toggle-fullscreen ()
  "Toggle the current frame between fullscreen and not."
  (interactive)
  (let ((fullscreen (frame-parameter nil 'fullscreen)))
    (if (not fullscreen)
        (set-frame-parameter nil 'fullscreen 'fullboth)
      (set-frame-parameter nil 'fullscreen nil))))

(defun bp-bury-scratch-buffer ()
  "Bury the scratch buffer on kill rather than killing it."
  (if (string= (buffer-name) "*scratch*")
      (ignore (bury-buffer))
    t))

(add-hook 'kill-buffer-query-functions #'bp-bury-scratch-buffer)


;; Use y and n instead of yes and no.
(defalias 'yes-or-no-p 'y-or-n-p)


;; Paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Home sweet home.
(setq default-directory (expand-file-name "~/"))

(defconst local-temp-dir (expand-file-name (locate-user-emacs-file "temp"))
  "The folder in which temp files should be stored.")


;; Environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package exec-path-from-shell
  :when (and (eq system-type 'darwin)
             (display-graphic-p))
  :load-path "vendor/exec-path-from-shell"
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LC_ALL" "LANG" "MANPATH" "GOPATH" "WORKON_HOME")))


;; Server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package server
  :when (display-graphic-p)
  :defer 5
  :config
  (server-start))


;; Themes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bp-remove-themes ()
  "Remove all of the themes that are currently enabled."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))

(defun bp-load-theme ()
  "Load a theme interactively, removing all other themes first."
  (interactive)
  (bp-remove-themes)
  (call-interactively #'load-theme))

(unless (display-graphic-p)
  (load-theme 'wombat t))

(use-package twilight-bright-theme
  :when (display-graphic-p)
  :load-path "vendor/twilight-bright-theme"
  :config (load-theme 'twilight-bright t))


;; EVIL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package queue :load-path "vendor" :defer t)

(use-package goto-chg
  :load-path "vendor/goto-chg"
  :commands goto-last-change)

(use-package undo-tree
  :disabled t
  :load-path "vendor/undo-tree"
  :diminish undo-tree-mode
  :commands global-undo-tree-mode
  :hook ((after-init . global-undo-tree-mode))
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diffs t
        undo-tree-history-directory-alist `((".*" . ,local-temp-dir))
        undo-tree-auto-save-history t))

(use-package evil
  :load-path "vendor/evil"
  :preface
  (defvar bp-evil-modes
    '(fundamental-mode beancount-mode conf-mode css-mode
                       evil-command-window-mode haskell-mode
                       haskell-cabal-mode hledger-mode json-mode
                       pollen-mode prog-mode text-mode sass-mode
                       typescript-mode web-mode yaml-mode)
    "The list of modes that should default to normal mode.  All modes
    derived from these will also default to evil normal mode.")

  (defvar bp-emacs-state-hooks
    '(git-commit-setup-hook
      git-timemachine-mode-hook
      macrostep-mode-hook
      magit-blame-mode-hook))

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

  (defun bp-open-terminal ()
    (interactive)
    (bp--open-app "Terminal"))

  (defun bp-open-firefox ()
    (interactive)
    (bp--open-app "Firefox"))

  (defun bp-open-safari ()
    (interactive)
    (bp--open-app "Safari"))
  :init
  (setq evil-search-module #'evil-search)
  :custom
  (evil-magic 'very-magic)
  (evil-vsplit-window-right t)
  (evil-undo-system 'undo-redo)
  :config
  (dolist (hook bp-emacs-state-hooks)
    (add-hook hook #'bp-toggle-emacs-state))

  (add-hook 'minibuffer-setup-hook #'bp-minibuffer-setup-hook)
  (add-hook 'after-change-major-mode-hook #'bp-apply-evil-mode-hook)
  (advice-add #'evil-generate-mode-line-tag :around #'bp-generate-mode-line-tag)
  (evil-mode)

  (bind-keys ("C-c C-\\" . evil-leader-prefix-map)
             ("C-c M-a"  . bp-open-terminal)
             ("C-c M-c"  . bp-open-firefox)
             ("C-c M-s"  . bp-open-safari)
             ("C-x C-k"  . kill-region)
             ("C-j"      . newline-and-indent)
             ("C-w"      . backward-kill-word)
             ("C--"      . text-scale-decrease)
             ("C-="      . text-scale-increase)
             ("C-+"      . text-scale-increase)
             ("H-f"      . bp-toggle-fullscreen))

  (bind-keys :map evil-insert-state-map
             ("C-x C-p" . evil-complete-previous-line)
             ("C-x C-n" . evil-complete-next-line))

  (bind-keys :map evil-visual-state-map
             :prefix "SPC"
             :prefix-map evil-leader-prefix-map
             ("c" . org-capture))

  (bind-keys :map evil-normal-state-map
             :prefix "SPC"
             :prefix-map evil-leader-prefix-map
             ("SPC" . recompile)
             ("\\"  . evil-ex-nohighlight)
             ("a"   . org-agenda)
             ("t"   . org-capture)
             ("i"   . bp-open-terminal)
             (",i"  . bp-find-init-file)
             ("c"   . org-capture)
             ("bu"  . browse-url)
             ("s"   . magit-status)
             ("m"   . mu4e)
             ("p"   . projectile-command-map)
             ("j"   . dumb-jump-go)
             ("xf"  . xref-find-definitions)
             ("ri"  . org-roam-node-insert)
             ("rf"  . org-roam-node-find)
             ("rc"  . bp-capture-roam-project-task)
             ("rt"  . org-roam-dailies-goto-today)
             ("ry"  . org-roam-dailies-goto-yesterday)))

(use-package evil-surround
  :load-path "vendor/evil-surround"
  :hook ((after-init . global-evil-surround-mode)))


;; Builtins ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package autorevert
  :hook ((after-init . global-auto-revert-mode)))

(use-package ansi-color
  :commands (ansi-color-apply-on-region))

(use-package compile
  :preface
  (defun bp-compilation-mode-hook ()
    (setq-local scroll-margin 0))

  (defun bp-colorize-compilation-buffer ()
    "Handle ANSI escape sequences in compilation buffer."
    (read-only-mode)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode))

  (defun bp-make-at (root-path)
    (interactive "fFilename: ")

    (let ((default-directory (locate-dominating-file buffer-file-name (f-filename root-path))))
      (compile "make")))

  (defun bp-make ()
    (interactive)
    (bp-make-at "Makefile"))

  (defun bp-recompile ()
    "Recompile all elisp sources."
    (interactive)
    (byte-recompile-directory (expand-file-name "~/.emacs.d/vendor") 0 t))
  :hook ((compilation-filter . bp-colorize-compilation-buffer)
         (compilation-mode   . bp-compilation-mode-hook))
  :config
  (setq compilation-scroll-output t))

(use-package dired
  :commands dired
  :config
  (setq insert-directory-program "/opt/local/bin/gls"
        dired-listing-switches "--group-directories-first -alh"
        dired-omit-files-p t
        dired-omit-files "^\\.?#\\|^__pycache__$"))

(use-package dired+
  :load-path "vendor"
  :after dired)

(use-package eldoc
  :diminish eldoc-mode
  :hook ((prog-mode . eldoc-mode)))

(use-package electric
  :hook ((org-mode  . electric-pair-local-mode)
         (prog-mode . electric-indent-mode)))

(use-package etags
  :defer 30
  :config
  (setq tags-add-tables nil
        tags-revert-without-query t))

(use-package files
  :config
  (setq auto-save-file-name-transforms `((".*" ,(concat local-temp-dir "/\\1") t))
        backup-directory-alist         `((".*" . ,local-temp-dir))
        backup-by-copying t))

(use-package hl-line
  :hook ((after-init . global-hl-line-mode)))

(use-package ido
  :bind (("C-x C-i" . imenu))
  :hook ((after-init . ido-mode))
  :custom
  (ido-everywhere t)
  :config
  (setq ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-max-prospects 10
        ido-ignore-extensions t
        ido-auto-merge-work-directories-length -1)

  (add-to-list 'ido-ignore-files "\\`compiled/")
  (add-to-list 'ido-ignore-files "\\`node_modules/")
  (add-to-list 'ido-ignore-files "\\`__pycache__/"))

(use-package ido-completing-read+
  :load-path "vendor/ido-completing-read+"
  :hook ((after-init . ido-ubiquitous-mode))
  :config
  (add-to-list 'ido-cr+-function-blacklist #'describe-function)
  (add-to-list 'ido-cr+-function-blacklist #'describe-variable))

(use-package ido-vertical-mode
  :load-path "vendor/ido-vertical-mode"
  :hook ((after-init . ido-vertical-mode)))

(use-package flx-ido
  :load-path "vendor/flx"
  :hook ((after-init . flx-ido-mode)))

(use-package smex
  :load-path "vendor/smex"
  :bind (("M-x" . smex)
         ("C-;" . smex)))

(use-package mule
  :preface
  (defun bp-mule-hook ()
    (interactive)
    (set-language-environment "utf-8")
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (prefer-coding-system 'utf-8))
  :hook ((after-init . bp-mule-hook)))

(use-package paren
  :hook ((prog-mode . show-paren-mode)))

(use-package re-builder
  :commands re-builder
  :config
  (setq reb-re-syntax 'rx))

(use-package recentf
  :preface
  (defvar bp-recentf-excludes
    '("/.virtualenvs/"
      "COMMIT_EDITMSG\\'"
      "MERGE_MSG\\'"
      "TAGS\\'"
      ".el.gz\\'"))
  :hook ((after-init . recentf-mode))
  :config
  (setq recentf-save-file (locate-user-emacs-file "recentf")
        recentf-max-saved-items 100
        recentf-max-menu-items 10
        recentf-auto-cleanup 60)
  (dolist (exclude bp-recentf-excludes)
    (add-to-list 'recentf-exclude exclude)))

(use-package savehist
  :hook ((after-init . savehist-mode))
  :config
  (setq savehist-file (locate-user-emacs-file "savehist")
        savehist-additional-variables '(search ring regexp-search-ring)))

(use-package saveplace
  :hook ((after-init . save-place-mode)))

(use-package simple
  :preface
  (setq bp-delete-trailing-whitespace-on t)
  (defun bp-delete-trailing-whitespace ()
    (interactive "")
    (message (format "delete-on: %s" bp-delete-trailing-whitespace-on))
    (when bp-delete-trailing-whitespace-on
      (call-interactively #'delete-trailing-whitespace)))
  :hook ((after-init  . line-number-mode)
         (after-init  . column-number-mode)
         (before-save . bp-delete-trailing-whitespace))
  :config
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))


;; Search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ag
  :load-path "vendor/ag"
  :commands (ag)
  :config
  (setq-default ag-ignore-list '("node_modules" "dist" "tmp" "vendor" "*.bundle.*" "*.html" "*.min.js" "*.rktd"))
  (setq ag-highlight-search t
        ag-reuse-buffers t))

(use-package grep
  :commands (grep rgrep)
  :config
  ;; Fish compatibility
  (grep-apply-setting 'grep-find-command '("find . -type f -exec grep -nH -e  \\{\\} \\+" . 34))
  (grep-apply-setting 'grep-find-template "find . <X> -type f <F> -exec grep <C> -inH -e <R> \\{\\} \\+"))


;; Git ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fullframe
  :load-path "vendor/fullframe"
  :commands (fullframe))

(use-package llama
  :load-path "vendor/llama"
  :defer t)

(use-package transient
  :load-path "vendor/transient/lisp"
  :defer t)

(use-package with-editor
  :load-path "vendor/with-editor/lisp"
  :defer t)

(use-package magit
  :load-path "vendor/magit/lisp"
  :commands (magit-status git-commit-mode)
  :mode (("COMMIT_EDITMSG\\'" . git-commit-mode)
         ("MERGE_MSG\\'"      . git-commit-mode))

  :config
  (setq magit-completing-read-function #'magit-ido-completing-read
        magit-repository-directories '(("~/sandbox" . 1)
                                       ("~/work"    . 1)))

  (fullframe magit-status magit-mode-quit-window))


;; Code Completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :load-path "vendor/company-mode"
  :hook ((org-mode  . company-mode)
         (prog-mode . company-mode))
  :diminish company-mode
  :config
  (setq company-idle-delay 0.3))

(use-package frame-local
  :disabled t
  :load-path "vendor/frame-local"
  :defer t)
(use-package company-box
  :disabled t
  :load-path "vendor/company-box"
  :hook ((company-mode . company-box-mode)))

(use-package editorconfig
  :load-path "vendor/editorconfig"
  :defer t)
(use-package copilot
  :load-path "vendor/copilot.el"
  :commands (copilot-mode)
  :diminish copilot-mode
  :preface
  (defun bp-copilot-mode-hook ()
    (company-mode -1))
  :hook ((copilot-mode . bp-copilot-mode-hook))
  :bind (:map copilot-completion-map
              ("TAB"   . copilot-accept-completion)
              ("M-TAB" . copilot-accept-completion-by-line)
              ("C-M-n" . copilot-next-completion)
              ("C-M-p" . copilot-previous-completion))
  :config
  (setq copilot-idle-delay 0.1
        copilot-node-executable (expand-file-name "~/bin/node20/bin/node")))


;; Linting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :load-path "vendor/flycheck"
  :hook ((prog-mode . flycheck-mode))
  :config
  (setq-default
   flycheck-standard-error-navigation nil ;; prevent flycheck from rebinding next-error (M-g n)
   flycheck-disabled-checkers '(python-pycompile racket sass scheme-chicken)
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-flake8rc "setup.cfg")
  (flycheck-add-mode 'javascript-eslint 'typescript-mode))


;; Navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dumb-jump
  :load-path "vendor/dumb-jump"
  :commands (dumb-jump-go dumb-jump-go-other-window)
  :hook ((prog-mode . dumb-jump-mode)))

(use-package projectile
  :load-path "vendor/projectile"
  :diminish projectile-mode
  :preface
  (defun bp-projectile-find-file-hook ()
    (let ((name (projectile-project-name)))
      (when (and (not (equal name bp-current-python-env))
                 (-contains-p (pyvenv-virtualenv-list) name))
        (bp-workon name)
        (normal-mode))))
  :hook ((after-init           . projectile-mode)
         (projectile-find-file . bp-projectile-find-file-hook))
  :config
  (setq projectile-enable-caching t))


;; BM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package bm-mode
  :load-path "~/work/blackmagic/blackmagic/lang/elisp"
  :mode (("\\.bm\\'" . bm-mode)))


;; binfmt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package binfmt-mode
  :load-path "~/sandbox/racket-binfmt/share"
  :mode (("\\.bnf\\'" . binfmt-mode)))


;; C ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :mode (("\\.c\\'"    . c-mode)
         ("\\.java\\'" . java-mode))
  :preface
  (defun bp-c-mode-hook ()
    (setq-local c-basic-offset 2)
    (setq-local c-default-style "bsd"))
  (defun bp-java-mode-hook ()
    (setq-local c-basic-offset 4)
    (setq-local c-default-style "java"))
  :config
  (setq c-basic-offset 2
        c-default-style "bsd")

  (add-hook 'c-mode-hook #'bp-c-mode-hook)
  (add-hook 'java-mode-hook #'bp-java-mode-hook))


;; Clojure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package parseclj :disabled t :load-path "vendor/parseclj" :defer t)
(use-package parseedn :disabled t :load-path "vendor/parseedn" :defer t)
(use-package sesman   :disabled t :load-path "vendor/sesman"   :defer t)
(use-package spinner  :disabled t :load-path "vendor/spinner"  :defer t)

(use-package clojure-mode
  :disabled t
  :load-path "vendor/clojure-mode"
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)))

(use-package cider
  :disabled t
  :load-path "vendor/cider"
  :hook ((clojure-mode . cider-mode))
  :config
  (setq cider-default-cljs-repl 'figwheel-main
        cider-figwheel-main-default-options ":dev"))


;; Docker ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dockerfile-mode
  :load-path "vendor/dockerfile-mode"
  :mode "\\Dockerfile\\'")

(use-package docker-tramp
  :disabled t
  :load-path "vendor/docker-tramp")


;; Extempore ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package extempore-mode
  :disabled t
  :load-path "vendor/extempore-emacs-mode"
  :mode "\\.xtm\\'"
  :config
  (setq extempore-path (expand-file-name "~/sandbox/extempore")))


;; Factor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package factor-mode
  :disabled t
  :load-path "~/sandbox/factor/misc/fuel"
  :mode "\\.factor\\'")

(use-package fuel-mode
  :disabled t
  :load-path "~/sandbox/factor/misc/fuel"
  :hook ((factor-mode . fuel-mode-hook))
  :config
  (setq fuel-factor-root-dir "~/sandbox/factor"))


;; Fish ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fish-mode
  :load-path "vendor/fish-mode"
  :mode "\\.fish\\'")


;; GDScript ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eglot
  :disabled t
  :bind (:map eglot-mode-map
              ("C-c ." . xref-find-definitions)
              ("C-c ," . xref-go-back)
              ("C-c ?" . xref-find-references)))

(use-package gdscript-mode
  :disabled t
  :load-path "vendor/gdscript-mode"
  :mode "\\.gd\\'"
  :hook ((gdscript-mode . bp-gdscript-setup))
  :preface
  (defun bp-gdscript-setup ()
    ;; https://github.com/godotengine/emacs-gdscript-mode/issues/128#issuecomment-1705685762
    (setq-local eglot-events-buffer-size 0))
  :config
  (setq gdscript-godot-executable "/Applications/Godot.app/Contents/MacOS/Godot"
        gdscript-gdformat-save-and-format t))


;; Go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package go-mode
  :load-path "vendor/go-mode"
  :mode "\\.go\\'"
  :config
  (setq gofmt-command "/Users/bogdan/go/bin/goimports")
  (setq flycheck-go-vet-executable "go")
  (setq flycheck-go-staticcheck-executable "/Users/bogdan/bin/staticcheck")
  (add-hook 'before-save-hook #'gofmt-before-save))


;; HCL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hcl-mode
  :load-path "vendor/emacs-hcl-mode"
  :mode ("\\.hcl\\'" "\\.nomad\\'" "\\.nomad.tpl\\'" "\\.tf\\'"))


;; JS/TS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bp-find-node-modules-root ()
  "Find the absolute path to the node_modules dir in the current project."
  (expand-file-name
   (locate-dominating-file (buffer-file-name) "node_modules")))

(defun bp-find-node-executable (name)
  "Find the executable named NAME in the current project's node_modules dir."
  (let* ((root (bp-find-node-modules-root))
         (bin-path (f-expand "node_modules/.bin" root))
         (exec-path (f-expand name bin-path)))
    (when (f-exists? exec-path)
      exec-path)))

(defun bp-eslint-setup ()
  "Set up paths and configuration for eslint."
  (interactive)
  (setq-local flycheck-javascript-eslint-executable (bp-find-node-executable "eslint")))

(defun bp-typescript-setup ()
  "Set up typescript linters."
  (interactive)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint))

(defun bp-nvm-use (ver)
  "Set up PATH for node VER from NVM."
  (interactive "sVersion: ")
  (let* ((sorted-versions (sort (mapcar #'car (nvm--installed-versions)) #'string<))
         (found-ver (-first (lambda (v)
                              (and (s-contains? "versions-" v)
                                   (s-contains? ver v)))
                            sorted-versions)))
    (if found-ver
        (progn
          (nvm-use found-ver)
          (message (format "Node version %s activated." ver)))
      (error (format "Version %s not found." ver)))))

(defun bp-nvm-hook ()
  "Set up PATH for node VER on a project basis."
  (interactive)
  (let* ((rc-path (locate-dominating-file (buffer-file-name) ".nvmrc"))
         (ver (when rc-path
                (with-temp-buffer
                  (insert-file-contents (format "%s/.nvmrc" rc-path))
                  (string-trim (buffer-string))))))
    (when ver
      (bp-nvm-use ver))))

(use-package js2-mode
  :load-path "vendor/js2-mode"
  :mode "\\.m?js\\'"
  :hook ((js2-mode . bp-nvm-hook)
         (js2-mode . bp-eslint-setup)
         (js2-mode . eldoc-mode))
  :config
  (setq js2-basic-offset 2
        js2-skip-preprocessor-directives t

        ;; Disable these because they clash w/ Flycheck.
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil

        ;; Enable maximum fontification.
        js2-highlight-level 3
        js2-highlight-external-variables t
        js2-idle-timer-delay 0.1))

(use-package rjsx-mode
  :disabled t
  :load-path "vendor/rjsx-mode"
  :mode "\\.m?jsx\\'"
  :hook ((rjsx-mode . bp-nvm-hook)
         (rjsx-mode . bp-eslint-setup)
         (rjsx-mode . eldoc-mode))
  :config
  (setq js2-basic-offset 2))

(use-package typescript-mode
  :load-path "vendor/typescript-mode"
  :mode "\\.tsx?\\'"
  :hook ((typescript-mode . bp-nvm-hook)
         (typescript-mode . bp-eslint-setup)
         (typescript-mode . bp-typescript-setup)
         (typescript-mode . eldoc-mode))
  :config
  (setq typescript-indent-level 2))

(use-package tide
  :load-path "vendor/tide"
  :after company
  :preface
  (defun bp-tide-hook ()
    (interactive)
    (tide-mode)
    (tide-setup))
  :commands (tide-mode tide-setup)
  :hook ((typescript-mode . bp-tide-hook)
         (typescript-mode . eldoc-mode))
  :bind (:map tide-mode-map
              ("C-M-x" . js-send-region)
              ("C-c ." . tide-jump-to-definition)
              ("C-c ," . tide-jump-back)))

(use-package nvm
  :load-path "vendor/nvm"
  :commands (nvm-use
             nvm--installed-versions)
  :after (dash s)
  :preface

  :config
  (setq nvm-dir (expand-file-name "~/.config/nvm")
        nvm-version-re "[0-9]+\.[0-9]+\.[0-9]+"))

(use-package prettier-js
  :load-path "vendor/prettier-js"
  :commands (prettier-js-mode)
  :diminish prettier-js-mode
  :preface
  (defun bp-prettier-js-mode ()
    (interactive)
    (setq-local prettier-js-command (or (bp-find-node-executable "prettier") "prettier"))
    (prettier-js-mode))
  :hook ((js2-mode rjsx-mode typescript-mode) . bp-prettier-js-mode))


;; JSON ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package json-snatcher :load-path "vendor/json-snatcher" :defer t)
(use-package json-reformat :load-path "vendor/json-reformat" :defer t)

(use-package json-mode
  :load-path "vendor/json-mode"
  :mode "\\.json\\'"
  :config
  (setq json-reformat:indent-width 2
        js-indent-level 2))


;; Lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lisp-mode
  :config
  (add-hook 'lisp-mode-hook #'slime-mode))

(use-package macrostep
  :load-path "vendor/macrostep"
  :commands (macrostep-expand))

(use-package paredit
  :load-path "vendor/paredit"
  :diminish paredit-mode
  :hook ((cider-repl-mode clojure-mode clojurescript-mode emacs-lisp-mode extempore-mode lisp-mode racket-mode scheme-mode) . paredit-mode)
  :bind (:map paredit-mode-map
              ("RET" . nil)
              ("C-j" . nil)))

(use-package rainbow-delimiters
  :load-path "vendor/rainbow-delimiters"
  :hook ((cider-repl-mode clojure-mode clojurescript-mode emacs-lisp-mode extempore-mode lisp-mode racket-mode scheme-mode) . rainbow-delimiters-mode))

(use-package sly
  :disabled t
  :load-path "vendor/sly"
  :commands (sly sly-mode)
  :config
  (setq inferior-lisp-program "ccl"))

(use-package slime
  :disabled t
  :load-path "vendor/slime"
  :commands (slime slime-mode)
  :init
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  :config
  (setq inferior-lisp-program "ccl"))

(use-package slime-company
  :disabled t
  :load-path "vendor/slime-company"
  :config
  (add-to-list 'company-backends #'company-slime)
  (setq slime-company-completion 'fuzzy))


;; Lua ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lua-mode
  :load-path "vendor/lua-mode"
  :mode "\\.lua\\'"
  :config
  (setq lua-indent-level 4))


;; Markdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package edit-indirect
  :load-path "vendor/edit-indirect")

(use-package markdown-mode
  :load-path "vendor/markdown-mode"
  :mode ("\\.md\\'" . gfm-mode))


;; Nginx ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package nginx-mode
  :load-path "vendor/nginx-mode"
  :mode "/nginx/.+")


;; Protobuf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package protobuf-mode
  :load-path "vendor"
  :mode "\\.proto\\'")


;; Python ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package pyvenv
  :load-path "vendor/pyvenv"
  :commands (pyvenv-virtualenv-list))

(use-package python
  :interpreter ("python" . python-mode)
  :mode (("\\.py\\'" . python-mode))
  :hook ((python-mode . bp-python-mode-hook))
  :preface
  (defvar bp-current-python-env nil)
  (defvar bp-black-python-envs '())
  (defvar bp-ruff-python-envs '("audiences" "servicebell" "blackmagic-client-python" "roas-pcapi-experiments"))

  (defun bp-python-mode-hook ()
    (interactive)
    (add-hook 'before-save-hook #'bp-python-format-before-save nil 'local))

  (defun bp-python-format-before-save ()
    (interactive)
    (py-isort-before-save)
    (when (member bp-current-python-env bp-black-python-envs)
      (let ((target "*black-format*"))
        (when (zerop (call-process-region
                      (point-min)
                      (point-max)
                      "black"
                      nil target nil
                      "-q" "-"))
          (replace-buffer-contents target))
        (kill-buffer target)))
    (when (member bp-current-python-env bp-ruff-python-envs)
      (let ((target "*ruff-format*"))
        (when (zerop (call-process-region
                      (point-min)
                      (point-max)
                      "ruff"
                      nil target nil
                      "format" "-"))
          (replace-buffer-contents target))
        (kill-buffer target))))

  (defun bp-read-virtualenv (prompt)
    "PROMPT for a virtualenv based on the list of known envs."
    (completing-read prompt (pyvenv-virtualenv-list) nil t nil 'pyvenv-workon-history nil nil))

  (defun bp-load-buffer-env (buffer-name)
    "Load an environment spec form a JSON buffer called BUFFER_NAME."
    (with-current-buffer buffer-name
      (goto-char (point-min))
      (dolist (binding (json-read))
        (let ((env (format "%s=%s" (car binding) (cdr binding))))
          (when (not (member env process-environment))
            (setq process-environment (cons env process-environment))))
        (when (eq (car binding) 'PATH)
          (setq exec-path (split-string (cdr binding) ":"))))))

  (defvar bp-export-python-env-template
    "vf activate %s; and python -c 'import json, os; print(json.dumps(dict(os.environ)))'")

  (defun bp-export-python-env (env env-buffer err-buffer)
    "Export the environment variables of ENV into ENV_BUFFER."
    (when (buffer-live-p env-buffer)
      (with-current-buffer env-buffer
        (erase-buffer)))

    (let ((cmd (format bp-export-python-env-template env)))
      (shell-command cmd env-buffer err-buffer)))

  (defun bp-workon (name)
    (interactive (list (bp-read-virtualenv "Work on: ")))
    (let ((env-buffer "*vf-activate*")
          (err-buffer "*vf-activate-errors*"))

      (bp-export-python-env name env-buffer err-buffer)
      (bp-load-buffer-env env-buffer)
      (setq bp-current-python-env name)
      (message (concat "Activated virtualenv " name)))))

(use-package py-isort
  :load-path "vendor/py-isort")

(use-package py-test
  :load-path "vendor/py-test"
  :config
  (evil-define-key 'normal python-mode-map
    "\\r" 'py-test-run-test-at-point
    "\\T" 'py-test-run-directory
    "\\t" 'py-test-run-file)

  (setq py-test-*mode-line-face-shenanigans-on* t
        py-test-*mode-line-face-shenanigans-timer* "0.5 sec"))

(use-package bp-py-test-projects
  :load-path "~/Documents/Org"
  :after py-test)


;; Scheme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package geiser-install
  :disabled t
  :config
  (setq geiser-scheme-implementation 'chicken)
  (setq geiser-active-implementations '(chicken))
  (setq geiser-default-implementation 'chicken))

(use-package scheme-mode
  :mode ("\\.scm" "\\.ss\\'" "\\.sls\\'" "\\.sps\\'")
  :init
  (put 'Trivit 'scheme-indent-function #'defun)
  (put 'constant-case 'scheme-indent-function #'defun)
  (put 'module 'scheme-indent-function #'defun)
  (put 'nanopass-case 'scheme-indent-function #'defun)
  (put 'with-output-language 'scheme-indent-function #'defun)
  (put 'with-syntax 'scheme-indent-function #'defun))


;; Racket ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package racket-hash-lang
  :load-path "vendor/racket-mode"
  :mode (("\\.rhm\\'" . racket-hash-lang-mode)
         ("\\.scrbl\\'" . racket-hash-lang-mode))
  :bind (:map racket-hash-lang-mode-map
              ("C-c C-d" . racket-xp-describe)
              ("C-c C-r" . racket-xp-rename)
              ("C-c ."   . xref-find-definitions)
              ("C-c ,"   . xref-go-back)))

(use-package racket-mode
  :load-path "vendor/racket-mode"
  :mode (("\\.rkt\\'" . racket-mode))
  :after flycheck
  :preface
  (defun bp-insert-lisp-section (section)
    "Insert a LISP section header with SECTION at point."
    (interactive "sSection: ")
    (let ((suffix (s-repeat (- 72 (length section) 4) ";")))
      (insert (format ";; %s %s\n" section suffix))))

  (defvar bp-racket-defun-likes
    '(call-with-browser!
      call-with-browser-script!
      call-with-database-connection
      call-with-database-transaction
      call-with-element-screenshot!
      call-with-encrypted-output
      call-with-hmac-output
      call-with-input-bytes
      call-with-input-string
      call-with-marionette!
      call-with-page
      call-with-page!
      call-with-page-pdf!
      call-with-page-screenshot!
      call-with-parameterization
      call-with-pdf-from-uri
      call-with-persistent-database-connection
      call-with-pk
      call-with-pool-connection
      call-with-pool-resource
      call-with-postmark-connection
      call-with-pubsub-events
      call-with-redis
      call-with-redis-client
      call-with-redis-pool
      call-with-redis-pubsub
      call-with-screenshot
      call-with-semaphore
      call-with-test-client+server
      call-with-transaction
      call-with-twilio-connection
      call-with-unzip
      call-with-waiter
      case/dep
      case/enum
      case-view
      for/stream
      form*
      fresh
      gen:let
      generator
      let*
      let-globals
      match-define-values
      place
      property
      section
      serializable-struct
      serializable-struct/versions
      struct++
      system-test-suite
      test
      test-commands
      tpl:xexpr-when
      unless~>
      xexpr-unless
      xexpr-when
      when~>
      λ2))

  (defun bp-racket-mode-hook ()
    (interactive)
    (setq adaptive-fill-mode t))

  (defun bp-racket-run-main ()
    (interactive)
    (racket--repl-run (cons (racket--buffer-file-name) '(main))))
  :config
  (add-hook 'racket-mode-hook #'bp-racket-mode-hook)

  (flycheck-define-checker racket-review
    "check racket source code using racket-review"
    :command ("raco" "review" source)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":error:" (message) line-end)
     (warning line-start (file-name) ":" line ":" column ":warning:" (message) line-end))
    :modes racket-mode)

  (add-to-list 'flycheck-checkers 'racket-review)

  (setq racket-repl-buffer-name-function #'racket-repl-buffer-name-project
        racket-show-functions '(racket-show-echo-area))

  (dolist (id bp-racket-defun-likes)
    (put id 'racket-indent-function #'defun))

  (setenv "PBRACKET_ROOT" (expand-file-name "~/sandbox/racket-pb"))
  (racket-add-back-end
   (expand-file-name "~/work/Podcatcher/podcatcher-core/")
   :racket-program (expand-file-name "~/work/Podcatcher/bin/pbracket"))
  (racket-add-back-end
   (expand-file-name "~/work/Podcatcher/podcatcher-test/")
   :racket-program (expand-file-name "~/work/Podcatcher/bin/pbracket"))

  (racket-add-back-end
   "/" ; the default back end for all local files
   :restart-watch-directories '("/Users/bogdan/sandbox/racket-gui-extra/gui-extra-lib/"))

  :bind (:map racket-mode-map
              ("{"       . paredit-open-curly)
              ("}"       . paredit-close-curly)
              ("C-c C-m" . bp-racket-run-main)
              ("C-c C-d" . racket-xp-describe)
              ("C-c C-r" . racket-xp-rename)
              ("C-c C-s" . bp-insert-lisp-section)
              ("C-c r t" . racket-tidy-requires)
              ("C-c r i" . racket-add-require-for-identifier)
              ("C-c ."   . xref-find-definitions)
              ("C-c ,"   . xref-go-back)))

(use-package racket-xp-mode
  :load-path "vendor/racket-mode"
  :hook ((racket-mode . racket-xp-mode)
         (racket-hash-lang-mode . racket-xp-mode)))

(use-package scribble-mode
  :disabled t
  :load-path "vendor/scribble-mode"
  :mode "\\.scrbl\\'")

(use-package pollen-mode
  :disabled t
  :load-path "vendor/pollen-mode"
  :mode "\\.p[mp]?\\'")


;; SASS/SCSS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2))

(use-package sass-mode
  :load-path "vendor/sass-mode"
  :mode (("\\.sass\\'" . sass-mode)
         ("\\.scss\\'" . scss-mode))
  :config
  (setq css-indent-offset 2))


;; SQL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sql
  :mode (("\\.sql\\'" . sql-mode))
  :preface
  (defun bp-sql-mode-hook ()
    (interactive)
    (sql-highlight-postgres-keywords))
  :hook ((sql-mode . bp-sql-mode-hook)))


;; SSH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ssh-config-mode
  :load-path "vendor/ssh-config-mode"
  :mode "/.ssh/config\\'")


;; Swift ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package swift-mode
  :load-path "vendor/swift-mode"
  :mode "\\.swift\\'"
  :preface
  (defun bp-swift-mode-hook ()
    (flycheck-swift-setup))
  (defun bp-xcode-run ()
    (interactive)
    (shell-command-to-string
     (concat
      "osascript"
      " -e 'tell application \"Xcode\"'"
      " -e 'set targetProject to active workspace document'"
      " -e 'stop targetProject'"
      " -e 'run targetProject'"
      " -e 'end tell'")))
  (defun bp-xcode-visit-file ()
    (interactive)
    (shell-command-to-string
     (concat "open -a /Applications/Xcode.app " (buffer-file-name))))
  :hook ((swift-mode . bp-swift-mode-hook))
  :config
  (setq swift-mode:basic-offset 2)
  :bind (:map swift-mode-map
              ("C-c C-r" . bp-xcode-run)
              ("C-c C-v" . bp-xcode-visit-file)))

(use-package flycheck-swift
  :load-path "vendor/flycheck-swift"
  :commands (flycheck-swift-setup))


;; TOML ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package toml-mode
  :load-path "vendor/toml-mode"
  :mode "\\.toml\\'")


;; Web ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :disabled t
  :load-path "vendor/web-mode"
  :after f
  :mode (("\\.html?\\'"        . web-mode)
         ("\\.mjml\\'"         . web-mode)
         ("\\.vue\\'"          . web-mode)
         ("\\.hbs\\'"          . web-mode)
         ("\\.eex\\'"          . web-mode)
         ("\\.tm?pl\\'"        . web-mode)
         ("\\.blade\\.php\\'"  . web-mode))
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-style-indent-offset 2
        web-mode-script-indent-offset 2
        web-mode-markup-indent-offset 2

        web-mode-style-padding 2
        web-mode-script-padding 2

        web-mode-enable-auto-closing t
        web-mode-enable-auto-expanding t
        web-mode-enable-auto-pairing t
        web-mode-enable-current-element-highlight t

        web-mode-content-types-alist '(("jsx" . "\\.mjs\\'"))
        web-mode-engines-alist '(("django" . "\\.html\\'"))))


;; Yaml ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :load-path "vendor/yaml-mode"
  :mode "\\.ya?ml\\'")


;; Beancount ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beancount
  :load-path "vendor/beancount"
  :mode (("\\.beancount\\'" . beancount-mode))
  :preface
  (defun bp-beancount-format-before-save ()
    (interactive)
    (let ((target "*beancount-format*"))
      (call-process-region
       (point-min)
       (point-max)
       (expand-file-name "~/Library/Python/3.12/bin/bean-format")
       nil target nil
       "-c" (number-to-string (+ beancount-number-alignment-column 2)))
      (replace-buffer-contents target)
      (kill-buffer target)))

  (defun bp-beancount-mode-hook ()
    (add-hook 'before-save-hook #'bp-beancount-format-before-save nil 'local))
  :config
  (setq beancount-number-alignment-column 54)
  (add-hook 'beancount-mode-hook #'bp-beancount-mode-hook)
  (add-hook 'beancount-mode-hook #'company-mode))


;; Mu4e ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mu4e
  :load-path "/Users/bogdan/sandbox/mu/build/mu4e"
  :commands (mu4e)
  :preface
  (defun bp-make-mu4e-matcher (mailbox-name addresses)
    (lexical-let ((addresses addresses)
                  (prefix (concat "/" mailbox-name "/")))
      (lambda (msg)
        (when msg
          (-any-p (lambda (addr)
                    (mu4e-message-contact-field-matches msg '(:from :to :cc) addr))
                  addresses)))))
  (defun bp-mu4e-filter-junk (q)
    (string-join `(,q
                   "flag:trashed"
                   "maildir:/business/junk"
                   "maildir:/personal/junk"
                   "maildir:/personal-archive/junk")
                 " AND NOT "))
  :bind (:map mu4e-main-mode-map
              ("q" . bury-buffer))
  :config
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
  (add-hook 'mu4e-compose-mode-hook #'company-mode)

  (setq
   sendmail-program "msmtp"
   message-send-mail-function #'message-send-mail-with-sendmail
   message-sendmail-extra-arguments '("--read-envelope-from")
   message-sendmail-f-is-evil t

   mu4e-mu-binary (expand-file-name "~/sandbox/mu/build/mu/mu")
   mu4e-attachment-dir "~/Downloads"

   mu4e-get-mail-command "mbsync -a"
   mu4e-update-interval 3600
   mu4e-change-filenames-when-moving t ;; prevents mbsync from complaining about duplicate UIDs
   mu4e-index-lazy-check nil
   gnus-blocked-images ".*"

   mu4e-bookmarks `((:name "All Inboxes"
                           :key ?i
                           :query ,(string-join '("maildir:/business/inbox"
                                                  "maildir:/personal/inbox"
                                                  "maildir:/personal-archive/inbox") " or "))
                    (:name "Unread Messages"
                           :key ?u
                           :query ,(bp-mu4e-filter-junk "flag:unread"))
                    (:name "Messages Today"
                           :key ?t
                           :query ,(bp-mu4e-filter-junk "date:today..now"))
                    (:name "Messages This Week"
                           :key ?w
                           :query ,(bp-mu4e-filter-junk "date:7d..now"))
                    (:name "Messages This Month"
                           :key ?m
                           :query ,(bp-mu4e-filter-junk "date:30d..now")))

   mu4e-view-actions '(("Thread"          . mu4e-action-show-thread)
                       ("View in Browser" . mu4e-action-view-in-browser))

   mu4e-context-policy 'pick-first
   mu4e-compose-context-policy 'ask-if-none
   mu4e-contexts (list (make-mu4e-context
                        :name "personal"
                        :match-func (bp-make-mu4e-matcher "personal" '("bogdan@defn.io"))
                        :vars '((user-mail-address           . "bogdan@defn.io")
                                (mu4e-refile-folder          . "/personal/archive")
                                (mu4e-sent-folder            . "/personal/sent")
                                (mu4e-drafts-folder          . "/personal/drafts")
                                (mu4e-trash-folder           . "/personal/trash")
                                (mu4e-sent-messages-behavior . sent)))

                       (make-mu4e-context
                        :name "matchacha"
                        :match-func (bp-make-mu4e-matcher "personal" '("bogdan@matchacha.ro" "hello@matchacha.ro"))
                        :vars '((user-mail-address           . "bogdan@matchacha.ro")
                                (mu4e-refile-folder          . "/personal/archive")
                                (mu4e-sent-folder            . "/personal/sent")
                                (mu4e-drafts-folder          . "/personal/drafts")
                                (mu4e-trash-folder           . "/personal/trash")
                                (mu4e-sent-messages-behavior . sent)))

                       (make-mu4e-context
                        :name "business"
                        :match-func (bp-make-mu4e-matcher "business" '("bogdan@cleartype.io" "bogdan@cleartype.ro" "bogdan@significantbit.io"))
                        :vars '((user-mail-address           . "bogdan@significantbit.io")
                                (mu4e-refile-folder          . "/personal/archive")
                                (mu4e-sent-folder            . "/personal/sent")
                                (mu4e-drafts-folder          . "/personal/drafts")
                                (mu4e-trash-folder           . "/personal/trash")
                                (mu4e-sent-messages-behavior . sent))))))

(use-package mu4e-alert
  :disabled
  :load-path "vendor/mu4e-alert"
  :commands (mu4e-alert-enable-mode-line-display
             mu4e-alert-enable-notifications
             mu4e-alert-set-default-style)
  :preface
  (defun bp-mu4e-alert-setup ()
    (interactive)
    (mu4e-alert-enable-mode-line-display)
    (mu4e-alert-enable-notifications)
    (mu4e-alert-set-default-style 'notifier)
    (setq mu4e-alert-interesting-mail-query (string-join '("flag:unread"
                                                           "flag:trashed"
                                                           "maildir:/business/junk"
                                                           "maildir:/personal/junk"
                                                           "maildir:/personal-archive/junk")
                                                         " AND NOT ")))
  :hook ((after-init . bp-mu4e-alert-setup)))


;; Org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :commands (org-agenda org-capture org-mode)
  :mode ("\\.org\\'" . org-mode)
  :preface
  (setq bp-scratch-file (expand-file-name "~/Documents/Org/scratch.org"))
  :config
  (defadvice org-goto (around ensure-emacs-state activate)
    (let ((prev-state evil-state))
      (evil-emacs-state)
      ad-do-it
      (evil-change-state prev-state)))
  (add-to-list 'org-agenda-files bp-scratch-file)
  (setq org-adapt-indentation t
        org-default-notes-file bp-scratch-file
        org-refile-targets `((,bp-scratch-file :maxlevel . 2))
        org-capture-templates '(("t" "todo" entry
                                 (file+headline bp-scratch-file "Tasks")
                                 "** TODO %^{Title}\n   %u\n   %a\n   %?"
                                 :clock-in :clock-keep :clock-resume))
        org-startup-folded 'overview))

(use-package emacsql
  :load-path "vendor/emacsql"
  :defer t)

(use-package org-roam
  :load-path "vendor/org-roam"
  :commands (org-roam-capture- org-roam-node-find)
  :preface
  (setq org-roam-directory (expand-file-name "~/Documents/Org/roam/"))

  (defun bp-make-roam-node-tag-predicate (tag)
    (lexical-let ((tag tag))
      #'(lambda (node)
          (member tag (org-roam-node-tags node)))))

  (defun bp-filter-roam-nodes-by-tag (tag)
    (seq-filter
     (bp-make-roam-node-tag-predicate tag)
     (org-roam-node-list)))

  (defun bp-build-agenda-files-list ()
    (interactive)
    (let ((nodes (bp-filter-roam-nodes-by-tag "project")))
      (let ((files (seq-uniq (mapcar #'org-roam-node-file nodes))))
        (setq org-agenda-files (cons bp-scratch-file files))
        (setq org-refile-targets (mapcar
                                  #'(lambda (file)
                                      `(,file :maxlevel . 2))
                                  org-agenda-files)))))

  (defun bp-capture-roam-project-task ()
    (interactive)
    (org-roam-capture-
     :node (org-roam-node-read nil (bp-make-roam-node-tag-predicate "project"))
     :templates '(("p" "a task for a project" entry
                   "** TODO %^{Title}\n   %u\n   %a\n   %?"
                   :target (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org" "* Tasks" ("Tasks"))
                   :clock-in :clock-keep :clock-resume))))

  :config
  (org-roam-db-autosync-mode)
  (bp-build-agenda-files-list)
  :hook
  ((org-capture-after-finalize-hook . bp-build-agenda-files-list)))

(use-package org-roam-dailies
  :load-path "vendor/org-roam/extensions"
  :commands (org-roam-dailies-capture-today
             org-roam-dailies-capture-yesterday
             org-roam-dailies-goto-today
             org-roam-dailies-goto-yesterday)
  :preface
  (setq org-roam-dailies-directory "dailies/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head
                    "%<%Y-%m-%d>.org"
                    "#+title: %<%Y-%m-%d>\n")))))

(use-package evil-org
  :load-path "vendor/evil-org-mode"
  :hook ((org-mode . evil-org-mode)))


(provide 'init)
;;; init.el ends here
