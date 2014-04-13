(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Refresh packages on first run.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Set-up and load packages.
(defconst my-packages
  '(;; Misc
    dired+ flycheck

    ;; VIM
    ;; evil commented out b/c I have my own fork
    auto-complete auto-complete-clang goto-last-change undo-tree yasnippet

    ;; Git
    git-gutter magit

    ;; Misc languages
    less-css-mode markdown-mode scala-mode2 web-mode yaml-mode

    ;; Lisp
    rainbow-delimiters rainbow-mode starter-kit starter-kit-lisp

    ;; Python
    jedi 

    ;; Themes
    twilight-theme)
  "A list of packages that must be installed.")

(defconst my-required-packages
  (append my-packages
          '(auto-complete-config cc-mode))
  "A list of packages that must be loaded.")

;; Install all packages that aren't already installed.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
  my-packages)

(mapc 'require my-required-packages)

(provide 'init-packages)
