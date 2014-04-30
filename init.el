;; UI
;; ~~
;; Font first.
(set-frame-font "Inconsolata-15")

;; Position and resize frame.
(when (window-system)
  (set-frame-position (selected-frame) 13 32)
  (set-frame-size (selected-frame) 234 65))

;; Remove GUI elements.
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Disable welcome screen.
(setq inhibit-startup-message t)


;; Initialize all of the other settings.
(add-to-list 'load-path (locate-user-emacs-file "config"))

(defconst my-modules
  '(;; Packages
    init-packages

    ;; Core
    init-core
    init-evil
    init-backup
    init-term
    init-org
    init-ido

    ;; Plugins
    init-auto-completion
    init-flycheck
    init-jedi
    init-web-mode

    ;; System
    init-git
    init-prodigy

    ;; Languages
    init-lang-c
    init-lang-elisp
    init-lang-haskell
    init-lang-javascript
    init-lang-python
    init-lang-racket
    init-lang-scala

    ;; Bindings
    init-bindings))

(mapc 'require my-modules)
