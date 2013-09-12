;;; Package archives
;;; ~~~~~~~~~~~~~~~~
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; Set-up and load packages.
(setq my-packages
     '(auto-complete
       dired+
       evil
       flymake-easy
       git-gutter
       goto-last-change
       ido-ubiquitous
       jedi
       jinja2-mode
       linum-relative
       magit
       markdown-mode
       monky
       paredit
       rainbow-delimiters
       rainbow-mode
       scala-mode2
       smex
       starter-kit
       twilight-theme
       undo-tree))

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
  my-packages)

(mapc
 (lambda (package)
   (require package))
  my-packages)

;;; Load ENSIME package.
(add-to-list 'load-path "~/.emacs.d/packages/ensime/elisp/")

;;; Jedi
;;; ~~~~
(add-hook 'python-mode-hook 'jedi:setup)

(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)

;;; VIM emulation
;;; ~~~~~~~~~~~~~
(evil-mode 1)

;;; Add C-w shortcut.
(global-set-key (kbd "C-w") 'backward-kill-word)

;;; Git
;;; ~~~
(global-git-gutter-mode t)

;;; Hide gutter if there are no changes.
(setq git-gutter:hide-gutter t)

;;; Scala mode
;;; ~~~~~~~~~~
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;; Flymake
;;; ~~~~~~~
(add-hook 'find-file-hook 'flymake-find-file-hook)

;;; Load pycheckers.
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name temp-file
                        (file-name-directory buffer-file-name))))
      (list "pycheckers" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;;; Fonts
;;; ~~~~~
(set-default-font "Inconsolata-15")

;;; Themes
;;; ~~~~~~
(load-theme 'twilight t)

;;; Auto completion
;;; ~~~~~~~~~~~~~~~
(global-auto-complete-mode t)

(setq ac-auto-start nil)

(ac-set-trigger-key "TAB")

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;;; Line editing
;;; ~~~~~~~~~~~~
(setq evil-default-cursor t)

;;; Highlight current line.
(global-hl-line-mode t)

;;; Don't wrap long lines.
(setq-default truncate-lines t)

;;; Make sure the cursor is white.
(set-cursor-color "#ffffff")

;;; Parantheses
;;; ~~~~~~~~~~~
(global-rainbow-delimiters-mode t)

;;; Backups
;;; ~~~~~~~
(setq make-backup-files nil)

;;; Misc
;;; ~~~~
(setq exec-path (append exec-path (list "/usr/local/bin")))
