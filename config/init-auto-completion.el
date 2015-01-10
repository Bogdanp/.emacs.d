;; AC
;; ~~
(use-package auto-complete
  :defer t
  :ensure t
  :init
  (progn
    ;; Auto-complete all the programming.
    (add-hook 'prog-mode-hook #'auto-complete-mode))
  :config
  (progn
    ;; Load AC's default configs.
    (require 'auto-complete-config)

    (ac-config-default)
    (ac-set-trigger-key "TAB")

    ;; Source ALL THE THINGS.
    (setq-default ac-sources '(ac-source-filename
                               ac-source-imenu
                               ac-source-features
                               ac-source-abbrev
                               ac-source-words-in-same-mode-buffers
                               ac-source-dictionary
                               ac-source-yasnippet))

    (setq ac-auto-start 3
          ac-auto-show-menu 0.25
          ac-quick-help-delay 0.25

          ac-use-menu-map t
          ac-use-fuzzy t
          ac-use-quick-help t)))


;; Clojure completion
;; ~~~~~~~~~~~~~~~~~~
(use-package ac-cider
  :ensure t
  :init
  (progn
    (add-hook 'cider-mode-hook #'ac-cider-setup)
    (add-hook 'cider-repl-mode-hook #'ac-cider-setup)))


(provide 'init-auto-completion)
