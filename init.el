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
  '(ace-jump-mode auto-complete dired+ evil expand-region
    flymake-cursor flymake-easy ghc git-gutter goto-last-change
    haskell-mode jedi jinja2-mode linum-relative markdown-mode monky
    rainbow-delimiters rainbow-mode scala-mode2 starter-kit
    starter-kit-lisp twilight-theme undo-tree yaml-mode)
  "A list of packages that must be installed and loaded.")

;; Install all packages that aren't already installed.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
  my-packages)

(mapc 'require my-packages)

;; Load ENSIME package.
(add-to-list 'load-path "~/.emacs.d/packages/ensime/elisp/")

;; Jedi
;; ~~~~
(add-hook 'python-mode-hook 'jedi:setup)

(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)

;; VIM emulation
;; ~~~~~~~~~~~~~
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
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Flymake
;; ~~~~~~~
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
            (unless (eq buffer-file-name nil) (flymake-mode 1))
            (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
            (local-set-key (kbd "M-n") 'flymake-goto-next-error)))

;; Fonts
;; ~~~~~
(set-default-font "Inconsolata-15")

;; Themes
;; ~~~~~~
(load-theme 'twilight t)

;; Auto completion
;; ~~~~~~~~~~~~~~~
(global-auto-complete-mode t)

;; Don't start automatically (causes SERIOUS performance issues on
;; large Python files (> 1k LOC)).
(setq ac-auto-start nil)

(ac-set-trigger-key "TAB")

;; Undo
;; ~~~~
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
(setq exec-path (append exec-path (list "/usr/local/bin")))

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
