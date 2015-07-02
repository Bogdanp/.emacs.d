;;; init.el --- main config entry point -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;;; GC
;; EMACS' default GC threshold is <1MB. Give it 200MB instead.
(setq gc-cons-threshold 200000000)


;;; auto-compile
(setq load-prefer-newer t)

(add-to-list 'load-path (locate-user-emacs-file "vendor/dash"))
(add-to-list 'load-path (locate-user-emacs-file "vendor/packed"))
(add-to-list 'load-path (locate-user-emacs-file "vendor/auto-compile"))
(add-to-list 'load-path (locate-user-emacs-file "private"))

(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)


;;; use-package
(add-to-list 'load-path (locate-user-emacs-file "vendor/use-package"))
(require 'use-package)
(require 'bind-key)


;;; UI
;; Position and resize frame.
(when (window-system)
  (add-to-list 'default-frame-alist '(font . "Fira Mono-10:antialias=none"))
  (add-to-list 'default-frame-alist '(top . 32))
  (add-to-list 'default-frame-alist '(left . 10))
  (add-to-list 'default-frame-alist '(width . 233))
  (add-to-list 'default-frame-alist '(height . 69)))

;; Remove GUI elements.
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Disable welcome screen.
(setq inhibit-startup-message t)


;;; Paths
;; Home sweet home.
(setq default-directory (expand-file-name "~/"))
(setq local-temp-dir (expand-file-name (locate-user-emacs-file "temp")))


;;; Packages
(with-no-warnings
  (require 'cl)
  (require 'package))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


(when (display-graphic-p)
  (use-package server
    :unless server-running-p
    :config (server-start))

  (defun bp-remove-themes ()
    (interactive)
    (mapcar #'disable-theme custom-enabled-themes))

  (defun bp-load-theme ()
    (interactive)
    (bp-remove-themes)
    (call-interactively #'load-theme))

  (use-package twilight-anti-bright-theme
    :load-path "vendor/twilight-anti-bright-theme"
    :config (load-theme 'twilight-anti-bright t))

  (use-package better-default-theme
    :disabled t
    :load-path "vendor/better-default-theme"
    :config (load-theme 'better-default t))

  (use-package smart-mode-line
    :commands (sml/setup)
    :ensure t
    :init
    (progn
      (setq sml/no-confirm-load-theme t)
      (sml/setup)

      (add-to-list 'sml/replacer-regexp-list '("^~/Work/" ":W:") t)
      (add-to-list 'sml/replacer-regexp-list '("^~/sandbox/" ":s:") t))))

(use-package evil
  :load-path "vendor/evil"
  :pin manual
  :preface
  (progn
    (defun my-default-to-emacs-mode-hook ()
      (evil-emacs-state))

    (defun my-toggle-emacs-mode-hook ()
      (if (equal evil-state 'emacs)
          (evil-normal-state)
        (evil-emacs-state)))

    (defun my-minibuffer-setup-hook-for-evil ()
      (local-set-key (kbd "C-w") 'backward-kill-word))

    (defun my-evil-local-mode-hook ()
      (setq-local interprogram-cut-function nil)
      (setq-local interprogram-paste-function nil)))
  :init
  (add-hook 'after-init-hook #'evil-mode)
  :config
  (progn
    (use-package ace-jump-mode
      :disabled t
      :diminish ace-jump-mode
      :ensure t
      :init
      (bind-keys :map evil-normal-state-map
                 ("C-c C-SPC"   . evil-ace-jump-word-mode)
                 ("C-c M-SPC" . evil-ace-jump-char-mode)))

    (use-package avy
      :ensure t
      :init
      (bind-keys :map evil-normal-state-map
                 ("C-c C-SPC"   . avy-goto-char)
                 ("C-c M-SPC" . avy-goto-line)))

    ;;; Fixes
    ;; Default to EMACS mode in these modes.
    (dolist (mode '(calendar-mode
                    comint-mode
                    compilation-mode
                    debugger-mode
                    diff-mode
                    dired-mode
                    erc-mode
                    eshell-mode
                    eww-mode
                    eww-bookmark-mode
                    eww-history-mode
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
                    magit-popup-mode
                    magit-reflog-mode
                    magit-refs-mode
                    magit-revision-mode
                    magit-stash-mode
                    magit-stashes-mode
                    magit-status-mode
                    monky-mode
                    special-mode
                    paradox-commit-list-mode
                    paradox-menu-mode
                    prodigy-mode
                    sbt-mode
                    swift-repl-mode
                    sx-compose-mode
                    sx-inbox-mode
                    sx-question-mode
                    sx-question-list-mode
                    term-mode
                    undo-tree-visualizer-mode
                    utop-mode))
      (evil-set-initial-state mode 'emacs))

    ;; Default to EMACS mode whenever these hooks are invoked.
    (dolist (hook '(flycheck-error-list-mode-hook
                    git-commit-setup-hook
                    git-timemachine-mode-hook))
      (add-hook hook #'my-default-to-emacs-mode-hook))


    ;; Toggle between emacs mode whenever these hooks are invoked.
    (dolist (hook '(magit-blame-mode-hook))
      (add-hook hook #'my-toggle-emacs-mode-hook))

    ;; Make C-w work in the minibuffer.
    (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook-for-evil)

    ;; Fix clipboard dirtying.
    (add-hook 'evil-local-mode-hook #'my-evil-local-mode-hook)

    ;; Fix copy-on-motion.
    (defadvice evil-visual-update-x-selection (around clobber-x-select-text activate)
      (fset 'old-x-select-text (symbol-function 'x-select-text))
      (fmakunbound 'x-select-text)
      ad-do-it
      (fset 'x-select-text (symbol-function 'old-x-select-text)))


    ;;; Bindings
    ;; "localleader"
    (bind-keys :map evil-normal-state-map
               ;; Compilation
               (",r" . bp-compile-with-default-command)
               (",R" . bp-compile-with-default-command-reset)

               ;; Misc
               (",," . evil-ex-nohighlight)
               (",x" . calc)
               (",v" . set-selective-display))

    ;; NORMAL mode
    (bind-keys :map evil-normal-state-map
               ;; Movement
               ("C-a" . evil-beginning-of-line)
               ("C-e" . evil-end-of-line)
               ("C-p" . evil-previous-line)
               ("C-n" . evil-next-line)

               ;; Windows
               ("C-w f" . bp-window-toggle-fullscreen))

    ;; INSERT mode
    (bind-keys :map evil-insert-state-map
               ("C-a" . beginning-of-line)
               ("C-e" . end-of-line)
               ("C-p" . evil-previous-line)
               ("C-n" . evil-next-line))

    ;; VISUAL mode
    (bind-keys :map evil-visual-state-map
               ("C-a" . evil-beginning-of-line)
               ("C-e" . evil-end-of-line)
               ("C-p" . evil-previous-line)
               ("C-n" . evil-next-line))))

(use-package evil-surround
  :ensure t
  :init
  (add-hook 'evil-mode-hook #'global-evil-surround-mode))

(use-package goto-chg
  :commands goto-last-change
  :ensure t)

(use-package undo-tree
  :diminish undo-tree-mode
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-undo-tree-mode)
  :config
  (progn
    (with-no-warnings
      (setq undo-tree-visualizer-timestamps t
            undo-tree-visualizer-diffs t
            undo-tree-history-directory-alist `((".*" . ,local-temp-dir))
            undo-tree-auto-save-history t))))

(use-package macrostep
  :bind ("C-c e m" . macrostep-expand)
  :ensure t)

(use-package magit
  :bind ("C-c m" . magit-status)
  :ensure t
  :init
  (setq magit-revert-buffers t)
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    (use-package fullframe
      :ensure t
      :config
      (fullframe magit-status magit-mode-quit-window))))

(use-package git-gutter
  :disabled t
  :commands global-git-gutter-mode
  :diminish git-gutter-mode
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-git-gutter-mode)
  :config
  (setq git-gutter:hide-gutter t))

(use-package git-timemachine
  :commands git-timemachine
  :ensure t)

(use-package monky
  :disabled t
  :commands monky-status
  :ensure t)

(use-package erc
  :commands erc
  :config
  (progn
    ;; Default config.
    (setq erc-server "irc.freenode.net"
          erc-port 6667
          erc-nick "bogdanp"
          erc-user-full-name user-full-name)

    ;; Highlight these things in incoming messages.
    (setq erc-keywords '("bogdanp"))

    ;; Autojoin these channels on freenode.
    (setq erc-autojoin-channels-alist
          '(("freenode.net" "#emacs" "#erc" "#haskell" "#python" "#scala"
             "#purescript" "#pixie-lang")))

    ;; Behave like a "normal" IRC client.
    (setq erc-kill-buffer-on-part t
          erc-kill-queries-on-quit t
          erc-kill-server-buffer-on-quit t)))

(use-package ido
  :commands ido-mode
  :init
  (add-hook 'after-init-hook #'ido-mode)
  :config
  (setq ido-enable-prefix nil
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-handle-duplicate-virtual-buffers 2
        ido-max-prospects 10
        ido-ignore-extensions t))

(use-package ido-ubiquitous
  :commands ido-ubiquitous-mode
  :ensure t
  :init
  (add-hook 'after-init-hook #'ido-ubiquitous-mode))

(use-package ido-vertical-mode
  :commands ido-vertical-mode
  :ensure t
  :init
  (add-hook 'after-init-hook #'ido-vertical-mode))

(use-package flx-ido
  :disabled t
  :commands flx-ido-mode
  :ensure t
  :init
  (add-hook 'after-init-hook #'flx-ido-mode))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :preface
  (progn
    (eval-when-compile
      (declare-function ibuffer-do-sort-by-alphabetic "ibuf-ext"))

    (defun my-ibuffer-hook ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))
  :init
  (add-hook 'ibuffer-hook #'my-ibuffer-hook))

(use-package ibuffer-vc
  :commands ibuffer-vc-set-filter-groups-by-vc-root
  :ensure t)

(use-package imenu
  :bind ("C-x C-i" . imenu))

(use-package imenu-anywhere
  :bind ("C-c C-i" . imenu-anywhere)
  :ensure t
  :config
  (require 'imenu))

(use-package smex
  :bind (("M-x" . smex)
         ("C-;" . smex))
  :commands smex-initialize
  :ensure t
  :init
  (add-hook 'after-init-hook #'smex-initialize)
  :config
  (setq smex-save-file (locate-user-emacs-file ".smex-items")))

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
       (sh      . t)))

    ;; Make org-babel work w/ these languages.
    (require 'ob-haskell)
    (require 'ob-latex)


    ;; Evaluate code in org files w/o asking for confirmation. Potentially
    ;; dangerous but meh.
    (setq org-confirm-babel-evaluate nil)


    ;;; Beamer
    (require 'ox-beamer)


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
    ;; Log the closing time of TODO items.
    (setq org-log-done 'time)

    ;; Better todo states.
    (setq org-todo-keywords
          '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

    ;; Refile anywhere.
    (setq org-refile-targets '((nil :maxlevel . 9)))


    ;;; Reminders
    ;; Code below mostly stolen from http://doc.norang.ca/org-mode.html#Reminders
    (defun bp-org-agenda-to-appt ()
      "Erase all current reminders and rebuild the list from the
    current agenda."
      (interactive)
      (setq appt-time-msg-list nil)
      (org-agenda-to-appt))

    ;; Plz don't ruin my window setup, org-agenda.
    (setq org-agenda-window-setup 'current-window)

    ;; Display appointment info in the modeline.
    (setq appt-display-mode-line t)
    (setq appt-display-format 'echo)

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
                               ac-source-dictionary))

    (setq ac-auto-start 5
          ac-auto-show-menu 0.15
          ac-quick-help-delay 0.15

          ac-use-menu-map t
          ac-use-fuzzy t
          ac-use-quick-help t)))

(use-package company
  :commands company-mode
  :diminish company-mode
  :ensure t
  :config
  (setq company-idle-delay 0.25))

(use-package yasnippet
  :disabled t
  :commands (yas-minor-mode yas-reload-all)
  :diminish yas-minor-mode
  :ensure t
  :config
  (yas-reload-all))

(use-package flycheck
  :commands flycheck-mode
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook #'flycheck-mode)

    (setq-default flycheck-disabled-checkers '(haskell-ghc
                                               html-tidy))

    (flycheck-define-checker jsxhint-checker
      "A JSX syntax and style checker based on JSXHint."

      :command ("jsxhint" source-inplace)
      :error-patterns
      ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
      :modes (web-mode))))

(use-package flycheck-haskell
    :commands flycheck-haskell-setup
    :ensure t)

(use-package autorevert
  :commands global-auto-revert-mode
  :init
  ;; Revert files that update on disk automatically. Ignores dirty buffers.
  (add-hook 'after-init-hook #'global-auto-revert-mode))

(use-package diminish
  :commands diminish
  :ensure t)

(use-package dired
  :commands dired
  :config
  (use-package dired+
    :ensure t))

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :commands exec-path-from-shell-initialize
    :ensure t
    :preface
    (defun my-shell-hook ()
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-env "GOPATH"))
    :init
    (add-hook 'after-init-hook #'my-shell-hook)))

(use-package f
  :defer t
  :ensure t)

(use-package fuzzy
  :defer t
  :ensure t)

(use-package paradox
  :commands paradox-list-packages
  :ensure t
  :config
  (setq paradox-github-token t))

(use-package recentf
  :commands recentf-mode
  :init
  (add-hook 'after-init-hook #'recentf-mode)
  :config
  (progn
    (setq recentf-save-file (locate-user-emacs-file "recentf")
          recentf-max-saved-items 1000
          recentf-max-menu-items 500)

    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")))

(use-package restclient
  :commands restclient-mode
  :ensure t)

(use-package savehist
  :commands savehist-mode
  :init
  (add-hook 'after-init-hook #'savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist")
        savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60

        history-length 1000))

(use-package saveplace
  :defer t
  :config
  (setq-default save-place t))

(use-package uniquify
  :defer t
  :config
  ;; /path/to/buffer instead of buffer<n>.
  (setq uniquify-buffer-name-style 'forward))

(use-package prodigy
  :bind (("C-c p" . prodigy))
  :ensure t
  :preface
  (eval-when-compile
    (declare-function prodigy-find-service "prodigy")
    (declare-function prodigy-start-service "prodigy")
    (declare-function prodigy-service-started-p "prodigy"))
  :config
  (progn
    (setq bp-prodigy-screenshot-service-env
          `(("PHANTOMJS_BIN_PATH" "/usr/local/bin/phantomjs")
            ("PHANTOMJS_CAPTURE_PATH" ,(expand-file-name "~/Work/screenshot-service/phantomjs/capture.js"))))

    (defun bp-prodigy-start-beanstalk& (k)
      (let ((beanstalkd (prodigy-find-service "beanstalkd")))
        (if (prodigy-service-started-p beanstalkd)
            (funcall k)
          (prodigy-start-service beanstalkd k))))

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

    (add-hook 'prodigy-view-mode-hook #'bp-prodigy-view-mode-hook)

    (use-package bp-prodigy-services)))

(use-package cc-mode
  :commands c-mode
  :config
  (progn
    (setq c-default-style "bsd"
          c-basic-offset 4 )

    ;; Fix indentation.
    (defun my-c-mode-hook ()
      (c-set-offset 'arglist-intro '+))

    (add-hook 'c-mode-hook 'my-c-mode-hook)))

(use-package irony
  :ensure t
  :preface
  (progn
    (defun my-irony-mode-hook ()
      ;; Disable AC since its irony mode isn't ready yet.
      (auto-complete-mode -1)

      (eldoc-mode +1)
      (irony-eldoc +1)
      (company-mode +1)))
  :init
  (add-hook 'c-mode-hook #'irony-mode)
  :config
  (progn
    (use-package company-irony
      :ensure t
      :preface
      (progn
        (defun my-company-irony-setup-hook ()
          (add-to-list 'company-backends 'company-irony)))
      :init
      (progn
        (add-hook 'irony-mode-hook #'my-company-irony-setup-hook)
        (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)))

    (use-package flycheck-irony
      :ensure t
      :preface
      (progn
        (defun my-flycheck-irony-setup-hook ()
          (add-to-list 'flycheck-checkers 'irony)))
      :init
      (add-hook 'irony-mode-hook #'my-flycheck-irony-setup-hook))

    (use-package irony-eldoc
      :commands irony-eldoc
      :ensure t)

    (add-hook 'irony-mode-hook #'my-irony-mode-hook)))

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
  :ensure t)

(use-package eldoc
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package ghc
  :ensure t
  :init
  (add-hook 'haskell-mode-hook #'ghc-init))

(use-package haskell-mode
  :mode "\\.l?hs\\'"
  :ensure t
  :preface
  (progn
    (eval-when-compile
      (declare-function haskell-process "haskell-process")
      (declare-function haskell-process-get-repl-completions "haskell-process"))

    (defun my-haskell-mode-hook ()
      (add-to-list 'ac-sources 'ac-source-haskell)

      (setq-local indent-line-function #'indent-relative)))
  :config
  (progn
    ;;; Config
    (setq haskell-process-common-args '("--ghc-option=-ferror-spans"
                                        "--ghc-option=-fno-warn-name-shadowing"
                                        "--ghc-option=-fno-warn-orphans"))

    (custom-set-variables
     ;; Haskell Process
     '(haskell-process-type 'cabal-repl)
     '(haskell-process-args-cabal-repl `(,@haskell-process-common-args))
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t)
     '(haskell-process-reload-with-fbytecode nil)
     '(haskell-process-use-presentation-mode t)
     '(haskell-process-show-debug-tips nil)

     ;; Haskell Interactive
     '(haskell-interactive-mode-do-fast-keys t)
     '(haskell-interactive-mode-eval-pretty nil)
     '(haskell-interactive-mode-include-file-name nil)

     ;; Misc
     '(haskell-stylish-on-save t)
     '(haskell-notify-p t)
     '(haskell-tags-on-save t))


    ;;; Auto complete
    (defun ac-haskell-candidates (prefix)
      (when (fboundp #'haskell-process-get-repl-completions)
        (let ((cs (haskell-process-get-repl-completions (haskell-process) prefix)))
          (-select (lambda (c) (not (string= "" c))) cs))))

    (ac-define-source haskell
      '((candidates . (ac-haskell-candidates ac-prefix))))

    (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
    (add-hook 'haskell-mode-hook #'haskell-doc-mode)
    (add-hook 'haskell-mode-hook #'my-haskell-mode-hook)
    (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)

    ;;; Bindings
    (bind-keys :map haskell-mode-map
               ("TAB"     . ac-complete)
               ("C-c C-z" . haskell-interactive-switch)
               ("C-c C-l" . haskell-process-load-or-reload)
               ("C-c C-t" . haskell-process-do-type)
               ("C-c C-i" . haskell-process-do-info)
               ("C-c v c" . haskell-cabal-visit-file))))

(use-package shm
  :ensure t
  :init
  (add-hook 'haskell-mode-hook #'structured-haskell-mode)
  :config
  (custom-set-variables
   '(shm-auto-insert-skeletons t)
   '(shm-auto-insert-bangs t)
   '(shm-use-hdevtools nil)
   '(shm-use-presentation-mode t)))

(use-package purescript-mode
  :ensure t
  :mode "\\.purs\\'")

(use-package paredit
  :diminish paredit-mode
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package markdown-mode
  :mode "\\.md\\'"
  :ensure t)

(use-package nim-mode
  :disabled t
  :mode "\\.nim\\'"
  :ensure t
  :config
  (progn
    (use-package ac-nim
      :commands ac-nim-enable
      :ensure t)
    (add-hook 'nim-mode-hook #'ac-nim-enable)))

(use-package tuareg
  :mode ("\\.mli?\\'" . tuareg-mode)
  :ensure t
  :config
  (progn
    (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
    (setq opam-lisp (concat opam-share "/emacs/site-lisp"))

    (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var)))

    (setq exec-path (append (parse-colon-path (getenv "PATH"))
                            (list exec-directory)))

    (use-package utop
      :ensure t)

    (use-package merlin
      :pin manual
      :load-path opam-lisp
      :commands (merlin-mode)
      :config
      (progn
        (setq merlin-use-auto-complete-mode 'easy)
        (setq merlin-command 'opam)))

    (use-package ocp-indent
      :pin manual
      :load-path opam-lisp
      :config
      (progn
        (setq ocp-indent-syntax '("lwt"))))

    (add-hook 'tuareg-mode-hook #'merlin-mode)
    (add-hook 'tuareg-mode-hook #'utop-minor-mode)))

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
         elpy-module-sane-defaults))))))

(use-package python
  :mode (("\\.py\\'"   . python-mode)
         ("SConstruct" . python-mode))
  :interpreter ("python" . python-mode)
  :preface
  (progn
    (eval-when-compile
      (declare-function py-test-define-project "py-test"))

    (defun my-python-mode-hook ()
      (auto-complete-mode -1)))
  :config
  (progn
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

    (add-hook 'python-mode-hook #'my-python-mode-hook)))

(use-package jedi
  :disabled t
  :commands jedi:setup
  :ensure t
  :preface
  (eval-when-compile
    (declare-function jedi:start-server "jedi-core")
    (declare-function jedi:stop-server "jedi-core"))
  :init
  (add-hook 'python-mode-hook #'jedi:setup)
  :config
  (progn
    (defun jedi:workon (path)
      (interactive "fVirtual env: ")
      (jedi:stop-server)
      (setq jedi:server-args
            `("--virtual-env" ,(expand-file-name path)))
      (jedi:install-server-block)
      (jedi:start-server)
      (jedi:setup))

    (defun jedi:workon-default ()
      (interactive)
      (jedi:stop-server)
      (setq jedi:server-args nil)
      (jedi:start-server)
      (jedi:setup))

    (bind-keys :map python-mode-map
               ("TAB" . jedi:complete))

    (bind-keys :map evil-normal-state-map
               (",jw" . jedi:workon)
               (",jd" . jedi:default))

    (setq jedi:complete-on-dot t
          jedi:tooltip-method nil)))

(use-package scala-mode2
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'"   . scala-mode))
  :ensure t)

(use-package sbt-mode
  :commands sbt-mode
  :ensure t)

(use-package ensime
  :commands ensime-scala-mode-hook
  :ensure t
  :init
  (add-hook 'scala-mode-hook #'ensime-scala-mode-hook)
  :config
  (progn
    (setq ensime-completion-style 'auto-complete
          ensime-default-java-flags '("-Xms512M" "-Xmx1G")
          ensime-sbt-command "activator")

    (bind-keys :map ensime-mode-map
               ("C-c C-." . ensime-edit-definition-other-window)
               ("C-c ."   . ensime-edit-definition)
               ("C-c ,"   . ensime-pop-find-definition-stack))))

(use-package sml-mode
  :mode "\\.\\(sml\\|sig\\)\\'"
  :ensure t)

(use-package swift-mode
  :disabled t
  :mode "\\.swift\\'"
  :ensure t
  :config
  (progn
    (add-to-list 'flycheck-checkers 'swift)))

(use-package less-css-mode
  :mode "\\.less\\'"
  :ensure t
  :config
  (progn
    (defun my-scss-mode-hook ()
      (setq-local css-indent-offset 2))

    (add-hook 'less-css-mode-hook 'my-scss-mode-hook)))

(use-package scss-mode
  :mode "\\.scss\\'"
  :ensure t
  :config
  (progn
    ;; Stupid functionality is stupid.
    (setq scss-compile-at-save nil)

    (defun my-scss-mode-hook ()
      (setq-local css-indent-offset 2))

    (add-hook 'scss-mode-hook 'my-scss-mode-hook)))

(use-package urweb-mode
  :load-path "/usr/local/share/emacs/site-lisp/urweb-mode"
  :mode "\\.ur[ps]?\\'")

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php\\'"   . web-mode)
         ("\\.hbs\\'"   . web-mode)
         ("\\.js\\'"    . web-mode))
  :config
  (progn
    (setq web-mode-code-indent-offset 4
          web-mode-style-indent-offset 4
          web-mode-script-indent-offset 4
          web-mode-markup-indent-offset 4

          web-mode-style-padding 4
          web-mode-script-padding 4

          web-mode-enable-auto-closing t
          web-mode-enable-auto-expanding t
          web-mode-enable-auto-pairing t
          web-mode-enable-current-element-highlight t

          web-mode-engines-alist '(("razor"  . "\\.scala\\.html\\'")
                                   ("django" . "\\.html\\'")))

    (defun my-web-mode-hook-for-flycheck ()
      (when (or (equal web-mode-content-type "javascript")
                (equal web-mode-content-type "jsx"))
        (flycheck-select-checker 'jsxhint-checker)
        (flycheck-mode 1)))

    (add-hook 'web-mode-hook #'my-web-mode-hook-for-flycheck)))

(use-package yaml-mode
  :mode "\\.yaml\\'"
  :ensure t)

(use-package robot-mode
  :load-path "vendor/robot-mode"
  :mode "\\.robot\\'")

(use-package swiper
  :commands (ivy-read)
  :bind (("C-s" . swiper))
  :ensure t)

(use-package counsel
  :commands (counsel-git-grep)
  :ensure t
  :init
  (bind-keys :map evil-normal-state-map
             (",C" . counsel-git-grep)))

(use-package find-file-in-project
  :commands (find-file-in-project)
  :ensure t
  :init
  (bind-keys :map evil-normal-state-map
             (",F" . find-file-in-project)))

(use-package grep
  :config
  (progn
    (bind-keys :map evil-normal-state-map
               (",S" . rgrep))))

(use-package term
  :config
  (bind-keys :map term-raw-escape-map
             ("c"    . bp-term-add)
             ("\C-k" . bp-term-kill)
             ("\C-n" . bp-term-next)
             ("\C-p" . bp-term-prev)
             ("\C-y" . bp-term-clipboard-paste)))

(use-package go-mode
  :disabled t
  :mode ("\\.go\\'" . go-mode)
  :ensure t
  :preface
  (defun my-go-mode-hook ()
    (use-package go-autocomplete
      :ensure t
      :init
      (add-to-list 'ac-sources 'ac-source-go))

    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save))
  :config
  (progn
    (add-hook 'go-mode-hook #'my-go-mode-hook)

    (bind-keys :map go-mode-map
               ("C-c C-d" . godoc)
               ("C-c C-f" . gofmt)
               ("C-c C-g" . go-goto-imports))))

(use-package hippie-expand
  :bind (("M-/" . hippie-expand)))

(use-package projectile
  :commands (projectile-global-mode)
  :diminish projectile-mode
  :ensure t
  :init
  (progn
    (setq projectile-keymap-prefix (kbd "C-c M-p")
          projectile-enable-caching t)

    (add-hook 'after-init-hook #'projectile-global-mode))
  :config
  (progn
    (bind-keys :map evil-normal-state-map
               (",f" . projectile-find-file-dwim)
               (",p" . projectile-switch-project))))

(use-package eshell
  :preface
  (progn
    (defun eshell-clear-buffer ()
      "Clear terminal"
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input)))

    (defun my-eshell-mode-hook ()
      (local-set-key (kbd "C-l") 'eshell-clear-buffer)))
  :config
  (add-hook 'eshell-mode-hook #'my-eshell-mode-hook))

(use-package dash-at-point
  :ensure t
  :config
  (bind-keys :map evil-normal-state-map
             (",d" . dash-at-point)))

;;; Config
;; Initialize all of the other settings.
(add-to-list 'load-path (locate-user-emacs-file "config"))

(defconst my-modules
  '(init-core
    init-term
    init-bindings))

(mapc 'require my-modules)

(provide 'init)
;;; init.el ends here
