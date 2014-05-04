(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Refresh packages on first run.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install missing packages.
(defconst my-packages
  '(;; Misc
    dired+ flycheck command-log-mode

    ;; Auto completion
    auto-complete
    auto-complete-clang
    ac-geiser
    ac-nrepl
    ac-slime

    ;; Better ido
    ido-ubiquitous
    smex

    ;; Editing
    ace-jump-mode

    ;; VIM emulation
    goto-chg
    undo-tree

    ;; Git
    git-gutter magit

    ;; System
    prodigy

    ;; Misc languages
    js2-mode less-css-mode markdown-mode php-mode scala-mode2 web-mode yaml-mode

    ;; Haskell
    flycheck-haskell
    haskell-mode
    shm

    ;; Lisp
    paredit
    pretty-lambdada
    rainbow-delimiters
    rainbow-mode

    ;; Common Lisp
    slime

    ;; Clojure
    clojure-mode
    clojure-test-mode
    cider

    ;; Elisp
    litable

    ;; Python
    jedi

    ;; Racket
    geiser

    ;; Themes
    twilight-theme
    twilight-anti-bright-theme)
  "A list of packages that must be installed.")

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
  my-packages)

;; Require packages.
(defconst my-required-packages
  (append my-packages
          '(ansi-color
	    auto-complete-config
	    cc-mode
	    ffap
	    saveplace
	    uniquify))
  "A list of packages that must be loaded.")

(mapc 'require my-required-packages)


(provide 'init-packages)
