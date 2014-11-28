;; AC
;; ~~
(global-auto-complete-mode t)

;; Use default config.
(ac-config-default)
(ac-set-trigger-key "TAB")

;; Source ALL the things.
(setq-default ac-sources '(ac-source-filename
                           ac-source-imenu
                           ac-source-features
                           ac-source-abbrev
                           ac-source-words-in-same-mode-buffers
                           ac-source-dictionary
                           ac-source-yasnippet))

;; Smarcase completion.
(setq ac-auto-show-menu 0.01
      ac-quick-help-delay 0.1

      ac-use-menu-map t
      ac-use-fuzzy t
      ac-use-quick-help t)


(provide 'init-auto-completion)
