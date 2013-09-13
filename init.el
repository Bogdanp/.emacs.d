;; Package archives
;; ~~~~~~~~~~~~~~~~
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Set-up and load packages.
(defconst my-packages
  '(ace-jump-mode auto-complete dired+ evil expand-region
    flymake-cursor flymake-easy ghc git-gutter goto-last-change
    haskell-mode jedi jinja2-mode linum-relative markdown-mode monky
    rainbow-delimiters rainbow-mode scala-mode2 starter-kit
    twilight-theme undo-tree yaml-mode)
  "A list of packages that must be installed and loaded.")

;; Refresh packages on first run.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install all packages that aren't already installed.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
  my-packages)

;; `require` all packages.
(mapc
 (lambda (package)
   (require package))
  my-packages)

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

;; Add C-w shortcut.
(global-set-key (kbd "C-w") 'backward-kill-word)

;; Git
;; ~~~
(global-git-gutter-mode t)

;; Hide gutter if there are no changes.
(setq git-gutter:hide-gutter t)

;; Scala
;; ~~~~~
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

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

(setq ac-auto-start nil)

(ac-set-trigger-key "TAB")

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; Undo
;; ~~~~
(setq undo-tree-auto-save-history t)

;; Line editing
;; ~~~~~~~~~~~~
(blink-cursor-mode 1)

(setq evil-default-cursor t)

;; Highlight current line.
(global-hl-line-mode t)

;; Don't wrap long lines.
(setq-default truncate-lines t)

;; Make sure the cursor is white.
(set-cursor-color "#ffffff")

;; Parantheses
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
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c M") 'monky-status)
(global-set-key (kbd "C-c g") 'multi-occur-in-matching-buffers)
(global-set-key (kbd "C-c f p") 'flymake-goto-prev-error)
(global-set-key (kbd "C-c f n") 'flymake-goto-next-error)

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
