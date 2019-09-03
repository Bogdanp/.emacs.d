;;; init.el --- main config entry point -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;;; Misc. builtin options
(setq
 ;;; Me
 user-full-name "Bogdan Popa"
 user-mail-address "bogdan@defn.io"

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
(add-to-list 'load-path (expand-file-name "~/Documents/Org"))


;;; auto-compile
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)


;;; use-package
(require 'use-package)


;;; UI
;; Position and resize frame.
(when (window-system)
  (add-to-list 'default-frame-alist '(font . "dank mono-12"))
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


;;; Set up PATH from shell
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))


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
    '(fundamental-mode beancount-mode conf-mode css-mode
                       evil-command-window-mode haskell-mode
                       haskell-cabal-mode hledger-mode json-mode
                       prog-mode text-mode sass-mode
                       typescript-mode web-mode yaml-mode)
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

  (defun bp-open-firefox ()
    (interactive)
    (bp--open-app "Firefox"))
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
                    magit-blame-mode-hook))
      (add-hook hook #'bp-toggle-emacs-state))

    (add-hook 'minibuffer-setup-hook #'bp-minibuffer-setup-hook)
    (add-hook 'after-change-major-mode-hook #'bp-apply-evil-mode-hook)
    (advice-add #'evil-generate-mode-line-tag :around #'bp-generate-mode-line-tag)
    (evil-mode +1)

    (bind-keys ("C-c C-\\" . evil-leader-prefix-map)
               ("C-c M-a"  . bp-open-terminal)
               ("C-c M-c"  . bp-open-firefox)
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
               ("i"   . bp-open-terminal)
               (",i"  . bp-find-init-file)
               ("a"   . org-agenda)
               ("c"   . org-capture)
               ("bu"  . browse-url)
               ("s"   . magit-status)
               ("m"   . mu4e)
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

    (add-to-list 'ido-ignore-files "\\`compiled/")
    (add-to-list 'ido-ignore-files "\\`node_modules/")
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

(use-package magit
  :ensure t
  :commands (magit-status git-commit-mode)
  :mode (("COMMIT_EDITMSG\\'" . git-commit-mode)
         ("MERGE_MSG\\'"      . git-commit-mode))
  :init
  (setq magit-completing-read-function #'magit-ido-completing-read
        magit-repository-directories '(("~/sandbox" . 1)
                                       ("~/work"    . 1)))
  :config
  (use-package fullframe
    :ensure t
    :config
    (fullframe magit-status magit-mode-quit-window)))

;;; Code completion
(use-package company
  :diminish company-mode
  :ensure t
  :init
  (progn
    (setq company-idle-delay 0.25)

    (add-hook 'after-init-hook #'global-company-mode)))


;;; Linting
(use-package flycheck
  :commands flycheck-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(python-pycompile racket sass))
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
        (bp-workon name)
        (normal-mode))))
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

 ;;; C
(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.java\\'" . java-mode))
  :preface
  (defun bp-java-mode-hook ()
    (setq-local c-basic-offset 4)
    (setq-local c-default-style "java"))
  :config
  (progn
    (setq c-basic-offset 4
          c-default-style "bsd")

    (add-hook 'java-mode-hook #'bp-java-mode-hook)))


;;; Clojure
(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :config
  (use-package cider
    :ensure t
    :config
    (setq cider-default-cljs-repl 'figwheel-main
          cider-figwheel-main-default-options ":dev")))


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
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'racket-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojurescript-mode-hook #'rainbow-delimiters-mode)
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
         ("\\.nomad\\'" . hcl-mode)
         ("\\.workflow\\'" . hcl-mode)))


;;; Haskell
(use-package intero
  :disabled
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


;;; Nginx
(use-package nginx-mode
  :ensure t)


;;; Python
(use-package pyvenv :ensure t)
(use-package python
  :mode (("\\.py\\'"   . python-mode))
  :interpreter ("python" . python-mode)
  :preface
  (defvar bp-current-python-env nil)

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
         '(elpy-rpc-python-command "python")
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

    (add-hook 'before-save-hook #'py-isort-before-save)))


;;; Racket, Scribble and Pollen
(use-package racket-mode
  :ensure t
  :mode ("\\.rkt\\'" . racket-mode)
  :preface
  (defun bp-racket-mode-hook ()
    (setq-local eldoc-documentation-function #'racket-eldoc-function))

  (defun bp-insert-lisp-section (section)
    "Insert a LISP section header with SECTION at point."
    (interactive "sSection: ")
    (let ((suffix (s-repeat (- 72 (length section) 4) ";")))
      (insert (format ";; %s %s\n" section suffix))))
  :config
  (add-hook 'racket-mode-hook #'bp-racket-mode-hook)

  ;; (flycheck-define-checker racket-konmari
  ;;   "check racket source code using konmari"
  ;;   :command ("raco" "konmari" "lint" source)
  ;;   :error-patterns
  ;;   ((error line-start (file-name) ":" line ":" column ":error:" (message) line-end)
  ;;    (warning line-start (file-name) ":" line ":" column ":warning:" (message) line-end))
  ;;   :modes racket-mode)

  (add-to-list 'flycheck-checkers 'racket-konmari)

  (put 'call-with-browser! 'racket-indent-function #'defun)
  (put 'call-with-browser-script! 'racket-indent-function #'defun)
  (put 'call-with-database-connection 'racket-indent-function #'defun)
  (put 'call-with-database-transaction 'racket-indent-function #'defun)
  (put 'call-with-element-screenshot! 'racket-indent-function #'defun)
  (put 'call-with-marionette! 'racket-indent-function #'defun)
  (put 'call-with-page! 'racket-indent-function #'defun)
  (put 'call-with-page-screenshot! 'racket-indent-function #'defun)
  (put 'call-with-persistent-database-connection 'racket-indent-function #'defun)
  (put 'call-with-pk 'racket-indent-function #'defun)
  (put 'call-with-postmark-connection 'racket-indent-function #'defun)
  (put 'call-with-redis-client 'racket-indent-function #'defun)
  (put 'call-with-redis-pool 'racket-indent-function #'defun)
  (put 'call-with-screenshot 'racket-indent-function #'defun)
  (put 'call-with-semaphore 'racket-indent-function #'defun)
  (put 'call-with-test-client+server 'racket-indent-function #'defun)
  (put 'call-with-transaction 'racket-indent-function #'defun)
  (put 'call-with-twilio-connection 'racket-indent-function #'defun)
  (put 'for/stream 'racket-indent-function #'defun)
  (put 'form* 'racket-indent-function #'defun)
  (put 'let* 'racket-indent-function #'defun)
  (put 'place 'racket-indent-function #'defun)
  (put 'property 'racket-indent-function #'defun)
  (put 'serializable-struct 'racket-indent-function #'defun)
  (put 'serializable-struct/versions 'racket-indent-function #'defun)
  (put 'struct++ 'racket-indent-function #'defun)
  (put 'system-test-suite 'racket-indent-function #'defun)
  (put 'test 'racket-indent-function #'defun)
  (put 'test-commands 'racket-indent-function #'defun)
  (put 'tpl:xexpr-when 'racket-indent-function #'defun)
  (put 'xexpr-when 'racket-indent-function #'defun)
  (bind-keys :map racket-mode-map
             ("C-c C-s" . bp-insert-lisp-section)
             ("C-c ."   . racket-visit-definition)
             ("C-c ,"   . racket-unvisit)))

(use-package scribble-mode
  :ensure t
  :mode ("\\.scrbl\\'" . scribble-mode))

(use-package pollen-mode
  :ensure t)


;;; SQL
(use-package sql
  :preface
  (defun bp-sql-mode-hook ()
    (sql-highlight-postgres-keywords))
  :config
  (add-hook 'sql-mode-hook #'bp-sql-mode-hook))


;;; SSH
(use-package ssh-config-mode
  :ensure t)


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

          web-mode-engines-alist '(("django" . "\\.html\\'")))

    (add-hook 'web-mode-hook #'bp-ts-web-mode-hook)
    (add-hook 'web-mode-hook #'bp-js-web-mode-hook)))


;;; Yaml
(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")


;;; beancount
(use-package beancount
  :mode ("\\.beancount\\'" . beancount-mode)
  :load-path "vendor/beancount")


;;; hledger
(use-package ledger-mode
  :ensure t
  :mode "\\.journal\\'"
  :config
  (setq ledger-default-date-format ledger-iso-date-format
        ledger-mode-should-check-version nil
        ledger-report-auto-width nil
        ledger-report-links-in-register nil
        ledger-report-use-native-highlighting nil
        ledger-report-use-strict nil
        ledger-reports '(("balances" "%(binary) -f %(ledger-file) bs -V")
                         ("monthly expenses" "%(binary) -f %(ledger-file) b expenses --tree -MV -b2018-10"))
        ledger-binary-path "hledger"))


;;; email
(use-package mu4e
  :load-path "/usr/local/Cellar/mu/HEAD-629badb_1/share/emacs/site-lisp/mu/mu4e"
  :preface
  (defun bp-make-mu4e-matcher (mailbox-name addresses)
    (lexical-let ((addresses addresses)
                  (prefix (concat "/" mailbox-name "/")))
      (lambda (msg)
        (when msg
          (-any-p (lambda (addr)
                     (mu4e-message-contact-field-matches msg '(:from :to :cc) addr))
                  addresses)))))

  (defun bp-capture-message (_)
    (call-interactively #'org-mu4e-store-and-capture))
  :config
  (progn
    (setq mu4e-mu-binary "/usr/local/bin/mu")

    (use-package smtpmail)
    (use-package subr-x)
    (use-package mu4e-alert
      :ensure t
      :config
      (mu4e-alert-enable-mode-line-display)
      (mu4e-alert-enable-notifications)
      (mu4e-alert-set-default-style 'notifier)
      (setq mu4e-alert-interesting-mail-query (string-join '("flag:unread"
                                                             "flag:trashed"
                                                             "maildir:/business/junk"
                                                             "maildir:/personal/junk"
                                                             "maildir:/personal-archive/junk")
                                                           " AND NOT ")))

    (bind-keys :map mu4e-main-mode-map
               ("q" . bury-buffer))

    (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

    (setq
     sendmail-program "msmtp"
     message-send-mail-function #'message-send-mail-with-sendmail
     message-sendmail-extra-arguments '("--read-envelope-from")
     message-sendmail-f-is-evil t

     mu4e-maildir "~/Mail"
     mu4e-attachment-dir "~/Downloads"

     mu4e-get-mail-command "mbsync -a"
     mu4e-update-interval 300
     mu4e-change-filenames-when-moving t ;; prevents mbsync from complaining about duplicate UIDs
     mu4e-index-lazy-check t
     mu4e-view-use-gnus t
     gnus-blocked-images ".*"

     mu4e-bookmarks '(((string-join '("maildir:/business/inbox"
                                      "maildir:/personal/inbox"
                                      "maildir:/personal-archive/inbox") " or ") "All Inboxes" ?i)
                      ((string-join '("flag:unread"
                                      "flag:trashed"
                                      "maildir:/business/junk"
                                      "maildir:/personal/junk"
                                      "maildir:/personal-archive/junk") " AND NOT ") "Unread Messages" ?u)
                      ("date:today..now" "Messages Today" ?t)
                      ("date:7d..now" "Messages This Week" ?w)
                      ("date:30d..now" "Messages This Month" ?m))

     mu4e-view-actions '(("Capture Message" . bp-capture-message)
                         ("Thread" . mu4e-action-show-thread)
                         ("View in Browser" . mu4e-action-view-in-browser))

     mu4e-context-policy 'pick-first
     mu4e-compose-context-policy 'ask-if-none
     mu4e-contexts `(,(make-mu4e-context
                       :name "personal"
                       :match-func (bp-make-mu4e-matcher "personal" '("bogdan@defn.io"))
                       :vars '((user-mail-address           . "bogdan@defn.io")
                               (mu4e-refile-folder          . "/personal/archive")
                               (mu4e-sent-folder            . "/personal/sent")
                               (mu4e-drafts-folder          . "/personal/drafts")
                               (mu4e-trash-folder           . "/personal/trash")
                               (mu4e-sent-messages-behavior . sent)))

                     ,(make-mu4e-context
                       :name "matchacha"
                       :match-func (bp-make-mu4e-matcher "personal" '("bogdan@matchacha.ro" "hello@matchacha.ro"))
                       :vars '((user-mail-address           . "bogdan@matchacha.ro")
                               (mu4e-refile-folder          . "/personal/archive")
                               (mu4e-sent-folder            . "/personal/sent")
                               (mu4e-drafts-folder          . "/personal/drafts")
                               (mu4e-trash-folder           . "/personal/trash")
                               (mu4e-sent-messages-behavior . sent)))

                     ,(make-mu4e-context
                       :name "business"
                       :match-func (bp-make-mu4e-matcher "business" '("bogdan@cleartype.io" "bogdan@cleartype.ro"))
                       :vars '((user-mail-address           . "bogdan@cleartype.io")
                               (mu4e-refile-folder          . "/business/archive")
                               (mu4e-sent-folder            . "/business/sent")
                               (mu4e-drafts-folder          . "/business/drafts")
                               (mu4e-trash-folder           . "/business/trash")
                               (mu4e-sent-messages-behavior . sent))))

     mu4e-user-mail-address-list '("bogdan@cleartype.ro"
                                   "bogdan@cleartype.io"
                                   "bogdan@defn.io"
                                   "bogdan@matchacha.ro"
                                   "hello@matchacha.ro"))))

;;; org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :preface
  (setq bp-notes-file    (expand-file-name "~/Documents/Org/notes.org")
        bp-someday-file  (expand-file-name "~/Documents/Org/someday.org")
        bp-tasks-file    (expand-file-name "~/Documents/Org/tasks.org"))
  :config
  (progn
    (use-package org-agenda)
    (use-package org-mu4e)

    (setq org-agenda-files (list bp-notes-file bp-tasks-file)
          org-default-notes-file bp-notes-file

          org-refile-targets `((,bp-notes-file    :maxlevel . 1)
                               (,bp-someday-file  :maxlevel . 1)
                               (,bp-tasks-file    :maxlevel . 1))

          org-capture-templates '(("n" "Note" entry (file+headline bp-notes-file "Notes") "** %? %^G\n   %U\n   %a\n\n   %i\n")
                                  ("b" "Backlog" entry (file+headline bp-tasks-file "Backlog") "** TODO %? %^G\n   %U\n   %a\n\n   %i\n")
                                  ("t" "Todo" entry (file+headline bp-tasks-file "Todo") "** TODO %? %^G\n   %U\n   %a\n\n   %i\n")))))

(use-package org-mu4e
  :commands (org-mu4e-compose-org-mode org-mu4e-store-and-capture)
  :config
  (setq org-mu4e-convert-to-html t))

(provide 'init)
;;; init.el ends here
