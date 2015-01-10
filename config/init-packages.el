;; Packages
;; ~~~~~~~~
(require 'cl)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
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
   org

   ;; EVIL enhancements
   goto-chg

   ;; Ido
   flx-ido
   ido-ubiquitous
   ido-vertical-mode
   imenu-anywhere
   smex

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

   ;; C and C++
   irony
   irony-eldoc

   ;; Haskell
   ghc
   haskell-mode
   shm

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
   cider))

;; Packages that may or may not be available on disk.
(bp-add-local-package 'evil)
(bp-add-local-package 'evil-surround)

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
 '(evil-surround))

(mapc 'require
      (set-difference my-required-packages
                      my-deferred-packages))


(provide 'init-packages)
