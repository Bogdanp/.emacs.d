(add-to-list 'load-path (locate-user-emacs-file "config"))

(defconst my-modules
  '(;; Packages
    init-packages

    ;; Core
    init-core
    init-evil
    init-backup
    init-term

    ;; Plugins
    init-auto-completion
    init-flycheck
    init-jedi
    init-web-mode
    init-yas

    ;; System
    init-git
    init-prodigy

    ;; Languages
    init-lang-c
    init-lang-haskell
    init-lang-python
    init-lang-scala

    ;; Bindings
    init-bindings))

(mapc 'require my-modules)
