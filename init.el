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
 mac-mouse-wheel-smooth-scroll t)

(set-language-environment "utf-8")

(setq-default
 ;;; Custom
 ;; Ensure custom values are saved to an ignored file.
 custom-file (locate-user-emacs-file "custom.el")

 ;;; Editing
 ;; Never use tabs.
 indent-tabs-mode nil

 ;; When using tabs (Go), they should be 2 spaces long.
 tab-width 2

 ;; Don't wrap long lines
 truncate-lines t)

;;; Enable narrow to region functionality
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
  (add-to-list 'default-frame-alist '(font . "operator mono-12"))
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


(defun bp-bury-scratch-buffer ()
  "Bury the scratch buffer on kill rather than killing it."
  (if (string= (buffer-name) "*scratch*")
      (ignore (bury-buffer))
    t))

(add-hook 'kill-buffer-query-functions #'bp-bury-scratch-buffer)


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

  (use-package twilight-bright-theme
    :load-path "vendor/twilight-bright-theme"
    :config (load-theme 'twilight-bright t)))


;;; EVIL
(use-package evil
  :load-path "vendor/evil"
  :pin manual
  :preface
  (defvar bp-evil-modes
    '(fundamental-mode conf-mode css-mode evil-command-window-mode d-mode
                       groovy-mode haskell-mode haskell-cabal-mode hledger-mode
                       json-mode ponylang-mode prog-mode purescript-mode restclient-mode
                       rust-mode text-mode sass-mode tuareg-mode typescript-mode
                       web-mode yaml-mode)
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

  (defun bp-open-terminal ()
    (interactive)
    (bp--open-app "Terminal"))

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
                    git-timemachine-mode-hook))
      (add-hook hook #'bp-toggle-emacs-state))

    (add-hook 'minibuffer-setup-hook #'bp-minibuffer-setup-hook)
    (add-hook 'after-change-major-mode-hook #'bp-apply-evil-mode-hook)
    (advice-add #'evil-generate-mode-line-tag :around #'bp-generate-mode-line-tag)
    (evil-mode +1)

    (bind-keys ("C-c C-\\" . evil-leader-prefix-map)
               ("C-c M-a"  . bp-open-terminal)
               ("C-c M-c"  . bp-open-chrome)
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

    (bind-keys :map evil-normal-state-map
               :prefix "SPC"
               :prefix-map evil-leader-prefix-map
               ("SPC" . recompile)
               ("\\"  . evil-ex-nohighlight)
               ("i"   . bp-open-terminal)
               (",i"  . bp-find-init-file)
               ("bu"  . browse-url)
               ("p"   . projectile-command-map)
               ("xf"  . xref-find-definitions))))


;;; Builtins
(use-package autorevert
  :config
  (global-auto-revert-mode))

(use-package ansi-color)

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
  :init
  (setq compilation-scroll-output t)
  (add-hook 'compilation-filter-hook #'bp-colorize-compilation-buffer)
  (add-hook 'compilation-mode-hook #'bp-compilation-mode-hook))

(use-package dired
  :commands dired
  :init
  (setq insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "--group-directories-first -alh")
  :config
  (progn
    (use-package dired+
      :ensure t)

    (setq-default dired-omit-files-p t)
    (setq dired-omit-files "^\\.?#\\|^__pycache__$")))

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
        ag-ignore-list '("node_modules" "*.bundle.*" "dist" "tmp")
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
  :config (global-hl-line-mode))

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
    (use-package ido-completing-read+ :ensure t)
    (use-package ido-vertical-mode :ensure t)
    (use-package flx-ido :ensure t)

    (flx-ido-mode)
    (ido-everywhere)
    (ido-ubiquitous-mode +1)
    (ido-vertical-mode +1)

    (add-to-list 'ido-ignore-files "\\`__pycache__/")))

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
  :config
  (save-place-mode +1))

(use-package simple
  :init
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

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
(use-package git-timemachine
  :commands git-timemachine
  :ensure t)


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
  (setq-default flycheck-disabled-checkers '(racket sass))
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
  (add-hook 'after-init-hook #'projectile-mode)
  (add-hook 'projectile-find-file-hook #'bp-projectile-find-file-hook))


;;; Miscellaneous
(use-package s
  :ensure t)

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
  :mode (("\\.c\\'" . c-mode)
         ("\\.java\\'" . java-mode))
  :config
  (setq c-default-style "bsd"
        c-basic-offset 4)

  (bind-keys :map java-mode-map
             :prefix "C-c"
             :prefix-map java-mode-prefix-map
             ("," . meghanada-back-jump)
             ("." . meghanada-jump-declaration)))


;;; Docker
(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
  :ensure t)


;;; Emacs lisp
(use-package eldoc
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package paredit
  :diminish paredit-mode
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'racket-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'racket-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode))


;;; Fish
(use-package fish-mode
  :ensure t
  :mode "\\.fish\\'")


;;; HCL
(use-package hcl-mode
  :ensure t
  :mode (("\\.hcl\\'" . hcl-mode)
         ("\\.nomad\\'" . hcl-mode)))


;;; Haskell
(use-package intero
  :load-path "vendor/intero/elisp"
  :mode "\\.hs\\'"
  :config
  (progn
    (use-package hindent
      :ensure t
      :config
      (progn
        (setq hindent-reformat-buffer-on-save t)

        (add-hook 'haskell-mode-hook #'hindent-mode)))

    (add-hook 'haskell-mode-hook #'intero-mode)))


;;; Java and Groovy
(use-package meghanada
  :ensure t
  :preface
  (defun bp-java-mode-hook ()
    (setq-local c-basic-offset 2)
    (meghanada-company-enable))
  :config
  (setq meghanada-javac-xlint "-Xlint:all,-processing")

  (add-hook 'java-mode-hook #'meghanada-mode)
  (add-hook 'java-mode-hook #'bp-java-mode-hook))

(use-package gradle-mode
  :ensure t
  :mode (("\\.gradle\\'" . gradle-mode))
  :config
  (add-hook 'gradle-mode-hook #'lsp-java-enable))

(use-package groovy-mode
  :ensure t
  :mode (("\\.gradle\\'" . groovy-mode))
  :config
  (add-hook 'groovy-mode-hook #'gradle-mode))


;;; TOML
(use-package toml-mode
  :ensure t
  :mode (("\\.toml\\'". toml-mode)
         ("Pipfile" . toml-mode)))


;;; JSON
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (setq json-reformat:indent-width 2
        js-indent-level 2))


;;; SASS
(use-package sass-mode
  :ensure t
  :mode (("\\.sass\\'" . sass-mode)
         ("\\.scss\\'" . scss-mode))
  :init
  (setq css-indent-offset 2))


;;; Lua
(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :config
  (setq lua-indent-level 4))


;;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))


;;; Mermaid
(use-package mermaid
  :mode ("\\.mmd\\'" . mermaid-mode)
  :load-path "vendor/mermaid-mode")


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
      (setq elpy-rpc-python-command "python")
      (message (concat "Activated virtualenv " name))))
  :config
  (progn
    (use-package py-isort
      :ensure t)

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

    (add-hook 'before-save-hook #'py-isort-before-save)
    (add-hook 'python-mode-hook #'yas-minor-mode)))


;;; Racket and Scribble
(use-package racket-mode
  :ensure t
  :mode ("\\.rkt\\'" . racket-mode)
  :preface
  (defun bp-racket-mode-hook ()
    (setq-local eldoc-documentation-function #'racket-eldoc-function))
  :config
  (add-hook 'racket-mode-hook #'bp-racket-mode-hook)

  (put 'call-with-database-connection 'racket-indent-function #'defun)
  (put 'call-with-database-transaction 'racket-indent-function #'defun)
  (put 'call-with-transaction 'racket-indent-function #'defun)
  (bind-keys :map racket-mode-map
             ("C-c ." . racket-visit-definition)
             ("C-c ," . racket-unvisit)))

(use-package scribble-mode
      :ensure t
      :mode ("\\.scrbl\\'" . scribble-mode))


;;; Web
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'"        . web-mode)
         ("\\.mjml\\'"         . web-mode)
         ("\\.vue\\'"          . web-mode)
         ("\\.hbs\\'"          . web-mode)
         ("\\.eex\\'"          . web-mode)
         ("\\.tm?pl\\'"        . web-mode)
         ("\\.blade\\.php\\'"  . web-mode)
         ("\\.jsx?\\'"         . web-mode)
         ("\\.tsx\\'"          . web-mode))
  :preface
  (defun bp-find-node-modules-root ()
    (expand-file-name
     (locate-dominating-file (buffer-file-name) "node_modules")))

  (defun bp-find-node-executable (name)
    (require 'f)
    (let* ((root (bp-find-node-modules-root))
           (bin-path (f-expand "node_modules/.bin" root))
           (exec-path (f-expand name bin-path)))
      (when (f-exists? exec-path)
        exec-path)))

  (defun bp-setup-eslint ()
    (setq-local flycheck-javascript-eslint-executable (bp-find-node-executable "eslint")))

  (defun bp-setup-prettier ()
    (setq-local prettier-js-command (bp-find-node-executable "prettier")))

  (defun bp-ts-web-mode-hook ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (tide-setup)
      (flycheck-add-mode 'typescript-tslint 'web-mode)))

  (defun bp-js-web-mode-hook ()
    (when (string-equal "js" (file-name-extension buffer-file-name))
      (bp-setup-eslint)
      (bp-setup-prettier)
      (prettier-js-mode)
      (flycheck-add-mode 'javascript-eslint 'web-mode)
      (flycheck-select-checker 'javascript-eslint)
      (flycheck-mode)))
  :init
  (progn
    (use-package prettier-js :ensure t)

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

          web-mode-engines-alist '(("django" . "\\.html\\'")
                                   ("razor"  . "\\.scala\\.html\\'")
                                   ("blade"  . "\\.blade\\.")))

    (add-hook 'web-mode-hook #'bp-ts-web-mode-hook)
    (add-hook 'web-mode-hook #'bp-js-web-mode-hook)))


;;; Yaml
(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")


;;; hledger
(use-package hledger-mode
  :ensure t
  :mode "\\.journal\\'"
  :config
  (progn
    (add-to-list 'company-backends 'hledger-company)
    (setq hledger-currency-string "RON"
          hledger-jfile (expand-file-name "~/sandbox/accounting/personal/2018.journal"))))


(provide 'init)
;;; init.el ends here
