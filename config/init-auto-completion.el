;; AC
;; ~~
(global-auto-complete-mode t)

;; Use default config.
(require 'auto-complete-config)
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

(setq ac-auto-start 4
      ac-auto-show-menu 0.25
      ac-quick-help-delay 0.25

      ac-use-menu-map t
      ac-use-fuzzy t
      ac-use-quick-help t)


(provide 'init-auto-completion)
