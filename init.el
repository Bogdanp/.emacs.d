;; UI
;; ~~
;; Position and resize frame.
(when (window-system)
  (add-to-list 'default-frame-alist '(font . "Inconsolata-15"))
  (add-to-list 'default-frame-alist '(top . 32))
  (add-to-list 'default-frame-alist '(left . 10))
  (add-to-list 'default-frame-alist '(width . 205))
  (add-to-list 'default-frame-alist '(height . 53)))

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
    init-magit
    init-backup
    init-frame
    init-term
    init-org
    init-ido
    init-erc

    ;; Misc plugins
    init-auto-completion
    init-flycheck
    init-prodigy
    init-web-mode

    ;; Languages
    init-lang-c
    init-lang-clojure
    init-lang-elisp
    init-lang-haskell
    init-lang-javascript
    init-lang-python
    init-lang-purescript
    init-lang-scala
    init-lang-scss

    ;; Bindings
    init-bindings))

(mapc 'require my-modules)
