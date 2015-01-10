;;; init.el --- main config entry point -*- no-byte-compile: t -*-
;;; auto-compile
(setq load-prefer-newer t)

(add-to-list 'load-path (locate-user-emacs-file "packed"))
(add-to-list 'load-path (locate-user-emacs-file "auto-compile"))

(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)


;;; use-package
(add-to-list 'load-path (locate-user-emacs-file "use-package"))
(require 'use-package)


;; Remove GUI elements.
(add-hook 'window-setup-hook
          (lambda ()
            (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
              (when (fboundp mode) (funcall mode -1)))))

;; Disable welcome screen.
(setq inhibit-startup-message t)


;;; Paths
;; Home sweet home.
(setq default-directory "~/")
(setq local-temp-dir (expand-file-name (locate-user-emacs-file "temp")))


;;; Config
;; Initialize all of the other settings.
(add-to-list 'load-path (locate-user-emacs-file "config"))

(defconst my-modules
  '(init-packages
    init-core
    init-evil
    init-bindings))

(mapc 'require my-modules)
