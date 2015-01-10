;;; init.el --- main config entry point -*- no-byte-compile: t -*-
;; Auto compile everything
;; ~~~~~~~~~~~~~~~~~~~~~~~
(setq load-prefer-newer t)

(add-to-list 'load-path (locate-user-emacs-file "packed"))
(add-to-list 'load-path (locate-user-emacs-file "auto-compile"))

(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)


;; use-package
;; ~~~~~~~~~~~
(add-to-list 'load-path (locate-user-emacs-file "use-package"))
(require 'use-package)


;; UI
;; ~~
;; Position and resize frame.
(when (window-system)
  (add-to-list 'default-frame-alist '(font . "Inconsolata-13"))
  (add-to-list 'default-frame-alist '(top . 32))
  (add-to-list 'default-frame-alist '(left . 10))
  (add-to-list 'default-frame-alist '(width . 199))
  (add-to-list 'default-frame-alist '(height . 59)))

;; Remove GUI elements.
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Disable welcome screen.
(setq inhibit-startup-message t)


;; Config
;; ~~~~~~
;; Initialize all of the other settings.
(add-to-list 'load-path (locate-user-emacs-file "config"))

(defconst my-modules
  '(;; Packages
    init-packages

    ;; Core
    init-core
    init-evil
    init-backup
    init-frame
    init-term
    init-org
    init-ido
    init-erc
    init-eww
    init-projectile

    ;; Misc plugins
    init-auto-completion
    init-company
    init-flycheck
    init-prodigy
    init-web-mode

    ;; Languages
    init-lang-c
    init-lang-clojure
    init-lang-elisp
    init-lang-haskell
    init-lang-javascript
    init-lang-purescript
    init-lang-python
    init-lang-scala
    init-lang-scons
    init-lang-scss
    init-lang-swift

    init-modeline
    init-bindings))

(mapc 'require my-modules)
