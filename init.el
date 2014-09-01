;; UI
;; ~~
;; Adding this to the default-frame-alist ensures that _all_ frames use
;; the same font and not just the main one.
(add-to-list 'default-frame-alist '(font . "Inconsolata-15"))

;; Position and resize frame.
(when (window-system)
  (add-to-list 'default-frame-alist '(top . 32))
  (add-to-list 'default-frame-alist '(left . 10))
  (add-to-list 'default-frame-alist '(width . 235))
  (add-to-list 'default-frame-alist '(height . 65)))

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
    init-mc

    ;; Plugins
    init-auto-completion
    init-flycheck
    init-jedi
    init-web-mode
    init-openwith
    init-yasnippet

    ;; System
    init-git
    init-prodigy

    ;; Languages
    init-lang-c
    init-lang-elisp
    init-lang-haskell
    init-lang-javascript
    init-lang-python
    init-lang-scala
    init-lang-scss

    ;; Bindings
    init-bindings))

(mapc 'require my-modules)
