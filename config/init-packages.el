;; Packages
;; ~~~~~~~~
(require 'cl)
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/") t)
(package-initialize)

;; Refresh packages on first run.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages nil
  "A list of packages that must be installed.")

(defvar my-required-packages nil
  "A list of packages that must be loaded.")

(defvar my-deferred-packages nil
  "A list of packages that must get installed but are not required
immediately.")

(defmacro append-to-list (list &rest xs)
  "Concatenates the value of LIST with XS and stores the value in LIST."
  `(setq ,list (append ,list ,@xs)))

(defun bp-add-local-package (package)
  "Load PACKAGE from disk if it's available. Otherwise, add it to the
list of packages to be instaleld using package.el."
  (let* ((package-name (symbol-name package))
         (local-path (expand-file-name (concat "~/sandbox/" package-name))))
    (if (file-exists-p local-path)
        (progn
          (add-to-list 'load-path local-path)
          (add-to-list 'my-required-packages package))
      (add-to-list 'my-packages package))))

;; Install missing packages.
(append-to-list
 my-packages
 '(;; Misc
   f
   org
   htmlize
   restclient
   paradox
   expand-region
   diminish

   ;; Auto completion
   fuzzy  ;; required by ac-use-fuzzy
   auto-complete
   auto-complete-clang
   yasnippet

   ;; EVIL enhancements
   goto-chg
   undo-tree

   ;; Ido
   flx-ido
   ido-ubiquitous
   ido-vertical-mode
   imenu-anywhere
   smex

   ;; Code analysis
   flycheck
   flycheck-haskell

   ;; File management
   dired+
   projectile

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
   markdown-mode
   scss-mode
   web-mode
   yaml-mode

   ;; Haskell
   ghc
   haskell-mode
   shm

   ;; Scala
   ensime
   scala-mode2
   sbt-mode

   ;; Lisp
   paredit
   pretty-lambdada
   rainbow-delimiters

   ;; Python
   jedi
   py-test

   ;; Rust
   rust-mode

   ;; Clojure
   clojure-mode
   cider
   ac-cider))

;; Packages that may or may not be available on disk.
(bp-add-local-package 'evil)

(when (display-graphic-p)
  (bp-add-local-package 'twilight-anti-bright-theme))


(mapc
 (lambda (package)
   (when package
     (or (package-installed-p package)
         (package-install package))))
 my-packages)

;; Require packages.
(append-to-list
 my-required-packages
 (append my-packages
         '(ansi-color
           cc-mode
           ffap
           python
           saveplace
           uniquify)))

(append-to-list
 my-deferred-packages
 '(evil-surround
   undo-tree))

(mapc 'require
      (set-difference my-required-packages
                      my-deferred-packages))


(provide 'init-packages)
