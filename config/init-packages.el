;;; Setup
(with-no-warnings (require 'cl)
                  (require 'package))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Refresh packages on first run.
(when (not package-archive-contents)
  (package-refresh-contents))


;;; Theme
(when (display-graphic-p)
  (add-to-list 'load-path (expand-file-name "~/sandbox/twilight-anti-bright-theme"))
  (require 'twilight-anti-bright-theme))


;;; EVIL
(add-to-list 'load-path (expand-file-name "~/sandbox/evil"))
(require 'evil)

(use-package evil-surround
  :commands evil-surround-mode
  :ensure t
  :init
  (add-hook 'evil-mode-hook #'global-evil-surround-mode))

(use-package goto-chg
  :commands goto-last-change
  :ensure t)

(use-package undo-tree
  :commands global-undo-tree-mode
  :diminish undo-tree-mode
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-undo-tree-mode)
  :config
  (progn
    (setq undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diffs t
          undo-tree-history-directory-alist `((".*" . ,local-temp-dir))
          undo-tree-auto-save-history t)))


;;; Git
(use-package magit
  :bind ("C-c m" . magit-status)
  :diminish magit-auto-revert-mode
  :ensure t)

(use-package git-gutter
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


;;; Ido
(use-package ido-ubiquitous
  :ensure t
  :init
  (ido-ubiquitous-mode +1))

(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode +1))

(use-package flx-ido
  :ensure t)

(use-package imenu-anywhere
  :bind ("C-c C-i" . imenu-anywhere)
  :ensure t
  :config
  (require 'imenu))

(use-package smex
  :bind (("M-x" . smex)
         ("C-;" . smex))
  :ensure t
  :init
  (smex-initialize)
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))


;;; Org
(use-package org
  :ensure t
  :init
  (progn
    (setq
     ;;; Misc
     ;; Paths to my org files
     bp-org-dir (expand-file-name "~/Dropbox/Documents/Personal")
     bp-org-main-file (expand-file-name (concat bp-org-dir "/Bogdan.org"))


     ;;; Code blocks
     ;; Highlight code in BEGIN_SRC-END_SRC blocks.
     org-src-fontify-natively t)


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


    ;;; Capture
    ;; Where to put captured stuff.
    (setq org-default-notes-file bp-org-main-file)

    ;; Capture templates.
    (setq org-capture-templates
          '(("t" "TODO" entry (file+headline bp-org-main-file "Tasks")
             "%^{Effort}p* TODO %?\n  :PROPERTIES:\n  :Created:  %u\n  :Source:   %a\n  :END:"
             :clock-in t
             :clock-keep t)
            ("s" "Call" entry (file+olp bp-org-main-file "LeadPages" "Meetings")
             "%^{Effort}p* TODO %?\n  :PROPERTIES:\n  :Created:  %u\n  :END:"
             :clock-in t
             :clock-keep t)
            ("m" "Meeting" entry (file+olp bp-org-main-file "LeadPages" "Meetings")
             "%^{Effort}p* TODO %?\n  :PROPERTIES:\n  :Created:  %u\n  :END:")
            ("n" "Note" entry (file+headline bp-org-main-file "Notes")
             "* %?\n  :PROPERTIES:\n  :Created:  %u\n  :END:")))


    ;;; Agenda
    ;; Set up path to agenda files.
    (setq bp-org-agenda-files-path bp-org-dir)

    (when (file-exists-p bp-org-agenda-files-path)
      (setq org-agenda-files `(,bp-org-agenda-files-path)))


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

    ;; Rebuild reminders each time the agenda is displayed.
    (add-hook 'org-finalize-agenda-hook #'bp-org-agenda-to-appt 'append)

    ;; Activate appointments.
    (appt-activate t)

    ;; Reset appointments 1 minute after midnight.
    (run-at-time "24:01" nil #'bp-org-agenda-to-appt)

    ;; Setup appointments at startup.
    (bp-org-agenda-to-appt)


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
            (org-paste-subtree (+ 1 archive-level))))))


    ;;; Habits
    (require 'org-habit)


    ;;; Text editing
    (add-hook 'org-mode-hook #'auto-fill-mode)


    ;;; Bindings
    (bind-keys :map evil-normal-state-map
               (",a" . org-agenda)
               (",c" . org-capture)
               (",ta" . bp-org-archive-task-at-point))))


;;; Auto completion
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

    (setq ac-auto-start 3
          ac-auto-show-menu 0.25
          ac-quick-help-delay 0.25

          ac-use-menu-map t
          ac-use-fuzzy t
          ac-use-quick-help t)))

(use-package company
  :commands company-mode
  :diminish company
  :ensure t
  :config
  (setq company-idle-delay 0.25))


(use-package company-irony
  :commands (company-irony-setup-begin-commands)
  :ensure t
  :init
  (progn
    (defun my-company-irony-setup-hook ()
      (add-to-list 'company-backends 'company-irony))

    (add-hook 'irony-mode-hook #'my-company-irony-setup-hook)
    (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)))

(use-package yasnippet
  :commands yas-reload-all
  :diminish yas-minor-mode
  :ensure t
  :idle (yas-reload-all))


;;; Flycheck
(use-package flycheck
  :commands (flycheck-mode flycheck-define-checker)
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook #'flycheck-mode))

  (setq-default flycheck-disabled-checkers '(emacs-lisp
                                             emacs-lisp-checkdoc
                                             haskell-ghc
                                             html-tidy))

  (flycheck-define-checker jsxhint-checker
    "A JSX syntax and style checker based on JSXHint."

    :command ("jsxhint" source-inplace)
    :error-patterns
    ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
    :modes (web-mode)) )

(use-package flycheck-haskell
  :commands flycheck-haskell-setup
  :ensure t
  :init
  (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))

(use-package flycheck-irony
  :ensure t
  :config
  (progn
    (defun my-flycheck-irony-setup-hook ()
      (add-to-list 'flycheck-checkers 'irony))

    (add-hook 'irony-mode-hook #'my-flycheck-irony-setup-hook)))


;;; Miscellaneous
(use-package ace-jump-mode
  :commands (ace-jump-mode ace-jump-char-mode)
  :diminish ace-jump-mode
  :ensure t
  :init
  (bind-keys :map evil-normal-state-map
             ("SPC"   . ace-jump-mode)
             ("S-SPC" . ace-jump-char-mode)))

(use-package bind-key
  :commands (bind-key bind-key*)
  :ensure t)

(use-package diminish
  :commands diminish
  :ensure t)

(use-package dired+
  :ensure t)

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :init
    (exec-path-from-shell-initialize)))

(use-package f
  :defer t
  :ensure t)

(use-package fuzzy
  :defer t
  :ensure t)

(use-package grep
  :init
  (grep-apply-setting 'grep-command "grep -irnHI -e "))

(use-package paradox
  :commands paradox-list-packages
  :ensure t
  :config
  (setq paradox-github-token t))

(use-package projectile
  :commands projectile-global-mode
  :ensure t
  :init
  (add-hook 'after-init-hook #'projectile-global-mode)
  :config
  (setq projectile-enable-caching t))

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
  :init
  (setq-default save-place t))

(use-package uniquify
  :init
  ;; /path/to/buffer instead of buffer<n>.
  (setq uniquify-buffer-name-style 'forward))


;;; Prodigy
(use-package prodigy
  :bind (("C-c P" . prodigy))
  :commands (prodigy prodigy-define-service)
  :ensure t
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

    (prodigy-define-service
      :name "LeadPages Server"
      :command (expand-file-name "~/Work/lead-pages/runserver")
      :cwd (expand-file-name "~/Work/lead-pages/")
      :tags '(work)
      :stop-signal 'sigterm
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "LeadPages AWeber Server"
      :command (expand-file-name "~/Work/leadpages-integrations/AWeber/runserver")
      :cwd (expand-file-name "~/Work/leadpages-integrations/AWeber/")
      :tags '(work)
      :stop-signal 'sigterm
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "LeadPages Screenshot Service"
      :command (expand-file-name "~/Work/screenshot-service/venv/bin/python")
      :cwd (expand-file-name "~/Work/screenshot-service/")
      :args `(,(expand-file-name "~/Work/screenshot-service/service/app.py") "5000")
      :env  bp-prodigy-screenshot-service-env
      :tags '(work)
      :stop-signal 'sigterm
      :kill-process-buffer-on-stop t
      :init-async #'bp-prodigy-start-beanstalk&)

    (prodigy-define-service
      :name "LeadPages Screenshot Service Consumer"
      :command (expand-file-name "~/Work/screenshot-service/venv/bin/python")
      :cwd (expand-file-name "~/Work/screenshot-service/consumer/")
      :args `(,(expand-file-name "~/Work/screenshot-service/consumer/consumer.py"))
      :env bp-prodigy-screenshot-service-env
      :tags '(work)
      :stop-signal 'sigterm
      :kill-process-buffer-on-stop t
      :init-async #'bp-prodigy-start-beanstalk&)

    (prodigy-define-service
      :name "beanstalkd"
      :command "beanstalkd"
      :tags '(personal work)
      :stop-signal 'sigterm
      :kill-process-buffer-on-stop t)))


;;; Server
(when (memq window-system '(mac ns))
  (use-package server
    :init
    (unless (server-running-p)
      (server-start))))


;;; C and C++
(use-package cc-mode
  :init
  (progn
    (setq c-default-style "bsd"
          c-basic-offset 4 )

    ;; Fix indentation.
    (defun my-c-mode-hook ()
      (c-set-offset 'arglist-intro '+))

    (add-hook 'c-mode-hook 'my-c-mode-hook)))

(use-package irony
  :commands irony-mode
  :ensure t
  :init
  (add-hook 'c-mode-common-hook #'irony-mode)
  :config
  (progn
    (defun my-irony-mode-hook ()
      ;; Disable AC since its irony mode isn't ready yet.
      (auto-complete-mode -1)

      (eldoc-mode +1)
      (irony-eldoc +1)
      (company-mode +1))

    (add-hook 'irony-mode-hook #'my-irony-mode-hook)))

(use-package irony-eldoc
  :commands irony-eldoc
  :ensure t)


;;; Emacs lisp
(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))


;;; Haskell
(use-package ghc
  :commands ghc-init
  :ensure t
  :init
  (add-hook 'haskell-mode-hook #'ghc-init))

(use-package haskell-mode
  :mode ("\\.l?hs\\'" . haskell-mode)
  :ensure t
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
          (remove-if (lambda (c) (string= "" c)) cs))))

    (ac-define-source haskell
      '((candidates . (ac-haskell-candidates ac-prefix))))


    ;;; Pretty symbols
    (defun my-haskell-mode-prettify-symbols-hook ()
      (setf prettify-symbols-alist
            (append '(;;; Syntax
                      ("forall" . ?∀)
                      ("::"     . ?∷)
                      ("\\"     . ?λ)
                      ("->"     . ?→)
                      ("=>"     . ?⇒)

                      ;;; Functions
                      ;; Prefix
                      ("undefined"   . ?⊥)
                      ("not"         . ?¬)
                      ("and"         . ?∧)
                      ("or"          . ?∨)
                      ;; Infix
                      (">>="         . ?↪)
                      ("=<<"         . ?↩)
                      ("*"           . ?×)
                      (":="          . ?≔)
                      ("|-"          . ?⊢)
                      ("-|"          . ?⊣)
                      ("<="          . ?≤)
                      (">="          . ?≥)
                      ("=="          . ?≡)
                      ("/="          . ?≠)
                      ("&&"          . ?∧)
                      ("||"          . ?∨)
                      ("!!"          . ?‼)
                      ("`div`"       . ?÷)
                      ("`elem`"      . ?∈)
                      ("`notElem`"   . ?∉)
                      ("`union`"     . ?∪)
                      ("`intersect`" . ?∩))
                    prettify-symbols-alist)))

    (add-hook 'haskell-mode-hook #'prettify-symbols-mode)
    (add-hook 'haskell-mode-hook #'my-haskell-mode-prettify-symbols-hook)


    (defun my-haskell-mode-hook ()
      (add-to-list 'ac-sources 'ac-source-haskell)

      (setq-local indent-line-function #'indent-relative))

    (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
    (add-hook 'haskell-mode-hook #'haskell-doc-mode)
    (add-hook 'haskell-mode-hook #'my-haskell-mode-hook)


    ;;; Bindings
    (bind-keys :map haskell-mode-map
               ("TAB"     . ac-complete)
               ("C-c C-z" . haskell-interactive-switch)
               ("C-c C-l" . haskell-process-load-or-reload)
               ("C-c C-t" . haskell-process-do-type)
               ("C-c C-i" . haskell-process-do-info)
               ("C-c v c" . haskell-cabal-visit-file)))

(use-package shm
  :commands structured-haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'structured-haskell-mode)
  :config
  (custom-set-variables
   '(shm-auto-insert-skeletons t)
   '(shm-auto-insert-bangs t)
   '(shm-use-hdevtools nil)
   '(shm-use-presentation-mode t))))


;;; LISP
(use-package paredit
  :commands paredit-mode
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(use-package pretty-lambdada
  :commands pretty-lambda-mode
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'pretty-lambda-mode))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))


;;; Markdown
(use-package markdown-mode
  :commands markdown-mode
  :ensure t)


;;; Python
(use-package jedi
  :commands jedi:setup
  :ensure t
  :init
  (add-hook 'python-mode-hook #'jedi:setup)
  :config
  (progn
    (bind-keys :map python-mode-map
               ("TAB" . jedi:complete))

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
      (jedi:setup))))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
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

        (py-test-define-project
         :name "LeadPages"
         :python-command "python"
         :base-directory (expand-file-name "~/Work/lead-pages/")
         :test-runner (expand-file-name "~/Work/lead-pages/tests/unit/runner.py")
         :test-runner-arguments '("-sv")
         :working-directory (expand-file-name "~/Work/lead-pages/tests/unit/"))))

    (defun my-python-mode-hook ()
      (pretty-lambda-mode t)

      (jedi:setup)
      (setq-local jedi:complete-on-dot t)
      (setq-local jedi:tooltip-method nil)

      (yas-minor-mode)

      ;; Don't start automatically (causes SERIOUS performance issues on
      ;; large Python files (> 1k LOC)).
      (setq-local ac-auto-start nil))

    (add-hook 'python-mode-hook 'my-python-mode-hook)))


;;; Rust
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :ensure t)


;;; Scala
(use-package scala-mode2
  :commands scala-mode
  :ensure t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
    (add-to-list 'auto-mode-alist '("\\.sbt\\'"   . scala-mode))))

(use-package sbt-mode
  :commands sbt-mode
  :ensure t)

(use-package ensime
  :bind (("C-c C-." . ensime-edit-definition-other-window)
         ("C-c ." . ensime-edit-definition)
         ("C-c ," . ensime-pop-find-definition-stack))
  :commands ensime-scala-mode-hook
  :ensure t
  :init
  (add-hook 'scala-mode-hook #'ensime-scala-mode-hook)
  :config
  (progn
    (setq ensime-default-scala-version "2.11.2"
          ensime-default-java-flags '("-Xms256M" "-Xmx1G")
          ensime-sbt-command "activator")

    (defun my-ensime-mode-hook ()
      ;; Disable auto-complete-mode since ensime uses Company mode
      ;; now.
      (auto-complete-mode -1))

    (add-hook 'ensime-mode-hook #'my-ensime-mode-hook)))


;;; SCSS
(use-package scss-mode
  :commands scss-mode
  :ensure t
  :config
  (progn
    ;; Stupid functionality is stupid.
    (setq scss-compile-at-save nil)

    (defun my-scss-mode-hook ()
      (setq-local css-indent-offset 2))

    (add-hook 'scss-mode-hook 'my-scss-mode-hook)))


;;; Web
(use-package web-mode
  :commands web-mode
  :ensure t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.php\\'"   . web-mode))
    (add-to-list 'auto-mode-alist '("\\.hbs\\'"   . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'"    . web-mode)))
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

    (defun my-web-mode-hook ()
      (setq-local ac-auto-start nil)

      ;; These things break web-mode so we need to disable them.
      (remove-hook 'prog-mode-hook 'esk-pretty-lambdas t)
      (remove-hook 'prog-mode-hook 'esk-add-watchwords t)
      (remove-hook 'prog-mode-hook 'idle-highlight-mode t))

    (defun my-web-mode-hook-for-flycheck ()
      (when (or (equal web-mode-content-type "javascript")
                (equal web-mode-content-type "jsx"))
        (flycheck-select-checker 'jsxhint-checker)
        (flycheck-mode 1)))

    (add-hook 'web-mode-hook #'my-web-mode-hook)
    (add-hook 'web-mode-hook #'my-web-mode-hook-for-flycheck)))


;;; YAML
(use-package yaml-mode
  :commands yaml-mode
  :ensure t)


(provide 'init-packages)
