(require 'cl)
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
    org
    openwith

    ;; Auto completion
    auto-complete
    auto-complete-clang

    ;; EVIL enhancements
    goto-chg
    undo-tree

    ;; Ido
    ido-ubiquitous
    smex

    ;; Code analysis
    flycheck
    flycheck-haskell

    ;; File management
    dired+

    ;; Editing
    multiple-cursors

    ;; Movement
    ace-jump-mode

    ;; Git
    git-gutter
    git-timemachine
    magit

    ;; System
    exec-path-from-shell
    prodigy

    ;; Frontend Languages
    js2-mode
    less-css-mode
    markdown-mode
    sass-mode
    scss-mode
    web-mode
    yaml-mode

    ;; Haskell
    haskell-mode
    shm

    ;; Scala
    scala-mode2
    sbt-mode

    ;; Lisp
    paredit
    pretty-lambdada
    rainbow-delimiters
    rainbow-mode

    ;; Python
    jedi

    ;; Rust
    rust-mode

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

(defconst my-deferred-packages
  '(evil-surround)
  "A list of packages that must get installed but are not required immediately.")

(mapc 'require
      (set-difference my-required-packages
                      my-deferred-packages))


(provide 'init-packages)
