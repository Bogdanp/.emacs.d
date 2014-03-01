;; Package archives
;; ~~~~~~~~~~~~~~~~
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Refresh packages on first run.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Set-up and load packages.
(defconst my-packages
  '(;; Misc
    dired+ flymake-cursor flymake-easy

    ;; VIM
    ;; evil commented out b/c I have my own fork
    auto-complete auto-complete-clang goto-last-change undo-tree

    ;; Git
    git-gutter magit

    ;; Misc languages
    less-css-mode markdown-mode roy-mode scala-mode2 web-mode
    yaml-mode fsharp-mode

    ;; Haskell
    ;; Handled below in el-get.
    ;; haskell-mode flymake-haskell-multi ghc

    ;; Lisp
    rainbow-delimiters rainbow-mode starter-kit starter-kit-lisp

    ;; Python
    jedi 

    ;; Racket
    quack geiser ac-geiser

    ;; Themes
    twilight-theme)
  "A list of packages that must be installed.")

(defconst my-required-packages
  (append my-packages
          '(auto-complete-config))
  "A list of packages that must be loaded.")

;; Install all packages that aren't already installed.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
  my-packages)

(mapc 'require my-required-packages)

;; el-get
;; ~~~~~~
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))

;; Install el-get if it's not present.
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; Synchronize recipes.
(defun el-get-sync-recipes (overlay)
  (let* ((recipe-glob (locate-user-emacs-file (concat overlay "/recipes/*.rcp")))
         (recipe-files (file-expand-wildcards recipe-glob))
         (recipes (mapcar 'el-get-read-recipe-file recipe-files)))
    (mapcar (lambda (r) (add-to-list 'el-get-sources r)) recipes)
    (el-get 'sync (mapcar 'el-get-source-name recipes))))
(setq el-get-user-package-directory user-emacs-directory)

(el-get-sync-recipes "el-get-haskell")
(el-get-sync-recipes "el-get-user")

;; Load ENSIME package.
(add-to-list 'load-path "~/.emacs.d/packages/ensime/elisp/")

;; Start up in ~/.
(setq default-directory "~/")

;; web-mode
;; ~~~~~~~~
(add-hook 'web-mode-hook
          (lambda ()
            (remove-hook 'prog-mode-hook 'esk-pretty-lambdas)
            (remove-hook 'prog-mode-hook 'esk-add-watchwords)
            (remove-hook 'prog-mode-hook 'idle-highlight-mode)))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'"   . web-mode))

(setq web-mode-engines-alist
      '(("django"  . "\\.html\\'")))

(setq web-mode-style-padding 4)
(setq web-mode-script-padding 4)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-enable-current-element-highlight t)

;; Jedi
;; ~~~~
(add-hook 'python-mode-hook 'jedi:setup)

(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)

;; VIM emulation
;; ~~~~~~~~~~~~~
(add-to-list 'load-path "~/sandbox/evil/")
(require 'evil)
(evil-mode 1)

;; Git
;; ~~~
(global-git-gutter-mode t)

;; Hide gutter if there are no changes.
(setq git-gutter:hide-gutter t)

;; Scala
;; ~~~~~
(require 'ensime)

(defun scala:test-toggle-path (fp)
  "Test whether FP is one of '(mvn-source mvn-test play-source
play-test) and transform it into its pair (source to test and
vice-versa).

  (scala:test-toggle-path
    \"/home/Users/bogdan/sandbox/foo/src/main/scala/Main.scala\")
    => \"/home/Users/bogdan/sandbox/foo/src/test/scala/MainSpec.scala\"

  (scala:test-toggle-path
    \"/home/Users/bogdan/sandbox/foo/src/test/scala/MainSpec.scala\")
    => \"/home/Users/bogdan/sandbox/foo/src/main/scala/Main.scala\""

  (flet ((m (s) (string-match-p s fp))

         (rg (s r) (replace-regexp-in-string s r fp))
         (rs (s r) (replace-regexp-in-string "Spec\.scala$" ".scala" (rg s r)))
         (rt (s r) (replace-regexp-in-string "\.scala$" "Spec.scala" (rg s r)))

         (mvn-source-p  () (m "/src/main/scala/.*.scala"))
         (mvn-test-p    () (m "/src/test/scala/.*.scala"))

         (play-source-p () (m "/app/.*.scala"))
         (play-test-p   () (m "/test/.*.scala"))

         (mvn-test-to-source  () (rs "/src/test/" "/src/main/"))
         (mvn-source-to-test  () (rt "/src/main/" "/src/test/"))

         (play-test-to-source () (rs "/test/" "/app/"))
         (play-source-to-test () (rt "/app/" "/test/")))

    (cond ((mvn-source-p) (mvn-source-to-test))
          ((mvn-test-p) (mvn-test-to-source))
          ((play-source-p) (play-source-to-test))
          ((play-test-p) (play-test-to-source))
          (t nil))))

(defun scala:toggle-test ()
  "Toggle between a Scala spec file and its implementation."
  (interactive)
  (let* ((fp (buffer-file-name))
         (np (scala:test-toggle-path fp)))
    (if np
        (progn
          (mkdir (file-name-directory fp) t)
          (find-file np))
      (message "Could not determine correct path"))))

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c t") 'scala:toggle-test)))

;; Haskell
;; ~~~~~~~
;; The TAB trigger for ac doesn't work in haskell-mode so we need
;; this (+ it's a lot nicer than having to TAB manually).
(add-hook 'haskell-mode-hook (lambda ()
                               (setq ac-auto-start t)))

;; Don't warn about name shadowing.
(setq ghc-ghc-options '("-fno-warn-hi-shadowing"
                        "-fno-warn-name-shadowing"))

;; Python
;; ~~~~~~
;; Load pycheckers.
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name temp-file
                        (file-name-directory buffer-file-name))))
      (list "pycheckers" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(add-hook 'python-mode-hook
          (lambda ()
            ;; Don't start automatically (causes SERIOUS performance issues on
            ;; large Python files (> 1k LOC)).
            (setq ac-auto-start nil)

            (unless (eq buffer-file-name nil) (flymake-mode 1))
            (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
            (local-set-key (kbd "M-n") 'flymake-goto-next-error)))

;; Fonts
;; ~~~~~
(set-default-font "Inconsolata-15")

;; Themes
;; ~~~~~~
(load-theme 'twilight t)

;; GUI
;; ~~~
(when (window-system)
  (set-frame-position (selected-frame) 205 32)
  (set-frame-size (selected-frame) 185 65))

;; Auto completion
;; ~~~~~~~~~~~~~~~
(global-auto-complete-mode t)

;; Use default config.
(ac-config-default)
(ac-set-trigger-key "TAB")

;; C auto completion.
(defun ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang) ac-sources)))
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)

;; Junk
;; ~~~~
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))

;; Undo
;; ~~~~
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)

;; Line editing
;; ~~~~~~~~~~~~
(blink-cursor-mode 1)

;; Make the cursor customizable again.
(setq evil-default-cursor t)

;; Highlight current line.
(global-hl-line-mode t)

;; Make sure the cursor is white.
(set-cursor-color "#ffffff")

;; Don't wrap long lines.
(setq-default truncate-lines t)

;; Parentheses
;; ~~~~~~~~~~~
(global-rainbow-delimiters-mode t)

;; Backups
;; ~~~~~~~
(setq make-backup-files nil)

;; Misc
;; ~~~~
(setq exec-path (append exec-path (list "/usr/local/bin/"
                                        "/Users/bogdan/.cabal/bin/"
                                        "/Applications/Racket v6.0.0.1/bin/")))

;; Bindings
;; ~~~~~~~~
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c M") 'monky-status)
(global-set-key (kbd "C-c g") 'multi-occur-in-matching-buffers)
(global-set-key (kbd "C-c f p") 'flymake-goto-prev-error)
(global-set-key (kbd "C-c f n") 'flymake-goto-next-error)

;; Auto completion bindings
;; ~~~~~~~~~~~~~~~~~~~~~~~~
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; EVIL bindings
;; ~~~~~~~~~~~~~
(define-key evil-normal-state-map ",," 'ace-jump-mode)
(define-key evil-normal-state-map ",r" 'er/expand-region)
(define-key evil-normal-state-map ",p" 'flymake-goto-prev-error)
(define-key evil-normal-state-map ",n" 'flymake-goto-next-error)

;; Useful emacs bindings in all modes.
(define-key evil-normal-state-map "\C-a" 'evil-beginning-of-line)
(define-key evil-insert-state-map "\C-a" 'beginning-of-line)
(define-key evil-visual-state-map "\C-a" 'evil-beginning-of-line)
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-normal-state-map "\C-p" 'evil-previous-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)
(define-key evil-normal-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
(define-key evil-visual-state-map "\C-n" 'evil-next-line)

;; Make C-w work in the minibuffer.
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (local-set-key (kbd "C-w") 'backward-kill-word)))

;; Fixes the copy-on-motion bullshit.
(defadvice evil-visual-update-x-selection (around clobber-x-select-text activate)
  (fset 'old-x-select-text (symbol-function 'x-select-text))
  (fmakunbound 'x-select-text)
  ad-do-it
  (fset 'x-select-text (symbol-function 'old-x-select-text)))
