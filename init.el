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
  (add-to-list 'default-frame-alist '(font . "Fira Mono-12"))
  (add-to-list 'default-frame-alist '(top . 32))
  (add-to-list 'default-frame-alist '(left . 10))
  (add-to-list 'default-frame-alist '(width . 199))
  (add-to-list 'default-frame-alist '(height . 59)))

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

(defun bp-remove-themes ()
  "Remove all of the themes that are currently enabled."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))

(defun bp-load-theme ()
  "Load a theme interactively, removing all other themese first."
  (interactive)
  (bp-remove-themes)
  (call-interactively #'load-theme))

(when (display-graphic-p)
  (use-package server
    :unless server-running-p
    :config (server-start))

  (use-package twilight-bright-theme
    :ensure t
    :config (load-theme 'twilight-bright t))

  (use-package twilight-anti-bright-theme
    :disabled t
    :load-path "vendor/twilight-anti-bright-theme"
    :config (load-theme 'twilight-anti-bright t))

  (use-package better-default-theme
    :disabled t
    :load-path "vendor/better-default-theme"
    :config (load-theme 'better-default t)))

(use-package smart-mode-line
  :commands (sml/setup)
  :ensure t
  :init
  (progn
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme nil)
    (sml/setup)

    (add-to-list 'sml/replacer-regexp-list '("^~/Work/" ":W:") t)
    (add-to-list 'sml/replacer-regexp-list '("^~/sandbox/" ":s:") t)))

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
    ;;; Fixes
    ;; Default to EMACS mode in these modes.
    (dolist (mode '(calendar-mode
                    cfw:details-mode
                    comint-mode
                    compilation-mode
                    debugger-mode
                    diff-mode
                    dired-mode
                    elm-interactive-mode
                    elm-package-mode
                    erc-mode
                    eshell-mode
                    eww-mode
                    eww-bookmark-mode
                    eww-history-mode
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
                    monky-mode
                    special-mode
                    paradox-commit-list-mode
                    paradox-menu-mode
                    process-menu-mode
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

(use-package avy
  :ensure t
  :config
  (bind-keys :map evil-normal-state-map
             ("C-c C-SPC" . avy-goto-char)
             ("C-c M-SPC" . avy-goto-line)))

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

(use-package magit
  :bind ("C-c m" . magit-status)
  :ensure t
  :config
  (progn
    (setq magit-revert-buffers t)
    (setq magit-completing-read-function #'magit-ido-completing-read)
    (setq magit-last-seen-setup-instructions "1.4.0")
    (setq magit-push-always-verify nil)

    (use-package fullframe
      :ensure t
      :config
      (fullframe magit-status magit-mode-quit-window))))

(use-package git-timemachine
  :commands git-timemachine
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
  (progn
    (use-package ido-ubiquitous
      :ensure t)

    (use-package ido-vertical-mode
      :ensure t)

    (setq ido-enable-prefix nil
          ido-auto-merge-work-directories-length nil
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-use-virtual-buffers t
          ido-handle-duplicate-virtual-buffers 2
          ido-max-prospects 10
          ido-ignore-extensions t)


    (ido-ubiquitous-mode +1)
    (ido-vertical-mode +1)))

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
       (sh      . t)
       (dot     . t)))

    ;; Make org-babel work w/ these languages.
    (require 'ob-haskell)
    (require 'ob-latex)


    ;; Evaluate code in org files w/o asking for confirmation. Potentially
    ;; dangerous but meh.
    (setq org-confirm-babel-evaluate nil)


    ;;; Beamer
    ;; Based on http://orgmode.org/worg/exporters/beamer/ox-beamer.html
    (require 'ox-beamer)

    (add-to-list 'org-latex-classes
                 '("beamer"
                   "\\documentclass\[10pt\]\{beamer\}"
                   ("\\plain\{%s\}" . "\\plain\{%s\}")
                   ("\\section\{%s\}" . "\\section*\{%s\}")
                   ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                   ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

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
          ac-auto-show-menu 0.5
          ac-quick-help-delay 0.5

          ac-use-menu-map t
          ac-use-fuzzy nil
          ac-use-quick-help t)))

(use-package company
  :commands company-mode
  :diminish company-mode
  :ensure t
  :config
  (setq company-idle-delay 0.25))

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
          recentf-max-saved-items 100
          recentf-max-menu-items 50
          recentf-auto-cleanup 60)

    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :ensure t)

(use-package savehist
  :commands savehist-mode
  :init
  (add-hook 'after-init-hook #'savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist")
        savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60

        history-length 10000))

(use-package saveplace
  :config
  (setq-default save-place t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package prodigy
  :bind (("C-c p" . prodigy))
  :ensure t
  :config
  (progn
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
  :mode ("\\.c\\'" . c-mode)
  :config
  (progn
    (setq c-default-style "bsd"
          c-basic-offset 4)

    ;; Fix indentation.
    (defun my-c-mode-hook ()
      (c-set-offset 'arglist-intro '+))

    (use-package irony
      :ensure t
      :commands irony-mode
      :preface
      (progn
        (defun my-irony-mode-hook ()
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

    (add-hook 'c-mode-hook #'my-c-mode-hook)
    (add-hook 'c-mode-hook #'irony-mode)))

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
  :ensure t)

(use-package eldoc
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package ghc
  :disabled t
  :ensure t
  :init
  (add-hook 'haskell-mode-hook #'ghc-init))

(use-package haskell-mode
  :mode "\\.l?hs\\'"
  :ensure t
  :preface
  (progn
    (defun my-haskell-mode-hook ()
      (set-face-attribute 'shm-current-face nil :background "#EEE")
      (set-face-attribute 'shm-quarantine-face nil :background "#DDD")

      (setq-local indent-line-function #'indent-relative)))
  :config
  (progn
    (require 'haskell-interactive-mode)
    (require 'haskell-process)

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
    (add-hook 'haskell-mode-hook #'my-haskell-mode-hook)

    (bind-keys :map haskell-mode-map
               ("C-c M-l" . haskell-process-reload-devel-main))))

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
  :ensure t)

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
  :preface
  (defun my-scala-mode-hook ()
    (auto-complete-mode -1)
    (yas-minor-mode -1)
    (company-mode +1))
  :init
  (progn
    (add-hook 'scala-mode-hook #'ensime-scala-mode-hook)
    (add-hook 'scala-mode-hook #'my-scala-mode-hook))
  :config
  (progn
    (setq ensime-default-java-flags '("-Xms512M" "-Xmx1G")
          ensime-sbt-command "activator")

    (bind-keys :map ensime-mode-map
               ("C-c C-." . ensime-edit-definition-other-window)
               ("C-c ."   . ensime-edit-definition)
               ("C-c ,"   . ensime-pop-find-definition-stack))))

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

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php\\'"   . web-mode)
         ("\\.hbs\\'"   . web-mode))
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

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

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

    (defconst bp-term-shell "zsh"
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
    (defun my-server-visit-hook-for-term ()
      "Most of the time I call `emacsclient' I'll be toggled-into `bp-term-**'.

I don't want calling `emacsclient' to break that configuration so this
hook works around that by toggling out of that configuration before
switching to the new buffer."
      (let ((buffer (current-buffer)))
        (when bp-term-previous-window-configuration
          (bp-term-toggle)
          (switch-to-buffer buffer))))

    (add-hook 'server-visit-hook #'my-server-visit-hook-for-term)

    (bind-keys :map term-raw-escape-map
               ("c"    . bp-term-add)
               ("\C-k" . bp-term-kill)
               ("\C-n" . bp-term-next)
               ("\C-p" . bp-term-prev)
               ("\C-y" . bp-term-clipboard-paste))

    (bind-keys ("C-c M-a" . bp-term-toggle))))

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

(use-package dash-at-point
  :ensure t
  :config
  (bind-keys :map evil-normal-state-map
             (",d" . dash-at-point)))

(use-package elm-mode
  :load-path "vendor/elm-mode"
  :mode ("\\.elm\\'" . elm-mode)
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-ac))

(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'")

(use-package fish-mode
  :disabled t
  :ensure t
  :mode "\\.fish\\'")


(use-package calfw
  :ensure t
  :config
  (progn
    (require 'calfw-ical)
    (require 'calfw-org)

    (defun bp-open-calendar ()
      (interactive)
      (cfw:open-calendar-buffer
       :contents-sources
       (list
        (cfw:org-create-source "DarkGreen")
        (cfw:ical-create-source "work" (expand-file-name "~/Documents/bogdan@ave81.com.ics") "IndianRed")
        )))

    (bind-keys :map evil-normal-state-map
               (",k"  . bp-open-calendar))))


;;; Disabled packages
(use-package eshell
  :disabled t
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

(use-package cask
  :disabled t
  :ensure t)

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

(use-package monky
  :disabled t
  :commands monky-status
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

(use-package purescript-mode
  :disabled t
  :ensure t
  :mode "\\.purs\\'")

(use-package sml-mode
  :disabled t
  :mode "\\.\\(sml\\|sig\\)\\'"
  :ensure t)

(use-package swift-mode
  :disabled t
  :mode "\\.swift\\'"
  :ensure t
  :config
  (progn
    (add-to-list 'flycheck-checkers 'swift)))

(use-package urweb-mode
  :disabled t
  :load-path "/usr/local/share/emacs/site-lisp/urweb-mode"
  :mode "\\.ur[ps]?\\'")

(use-package yasnippet
  :disabled t
  :commands (yas-minor-mode yas-reload-all)
  :diminish yas-minor-mode
  :ensure t
  :config
  (yas-reload-all))


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

;; Wrap long lines.
(setq-default truncate-lines nil)

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


;;; Random bindings
(bind-keys ("C-j" . newline-and-indent)
           ("C-w" . backward-kill-word)
           ("C--" . text-scale-decrease)
           ("C-=" . text-scale-increase)
           ("C-+" . text-scale-increase))


(provide 'init)
;;; init.el ends here
