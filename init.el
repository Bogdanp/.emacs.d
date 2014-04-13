(add-to-list 'load-path (locate-user-emacs-file "config"))

(defconst my-modules
  '(;; Packages
    init-packages

    ;; Core
    init-core
    init-evil
    init-backup
    init-auto-completion

    ;; Plugins
    init-flycheck
    init-jedi
    init-web-mode

    ;; Tools
    init-git

    ;; Languages
    init-lang-c
    init-lang-haskell
    init-lang-python
    init-lang-scala

    init-bindings))

(mapc 'require my-modules)
