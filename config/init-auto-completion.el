(global-auto-complete-mode t)

;; Use default config.
(ac-config-default)
(ac-set-trigger-key "TAB")

;; I don't mind having to press TAB.
(setq ac-auto-start nil)

;; Source ALL the things.
(setq-default ac-sources '(ac-source-filename
                           ac-source-functions
                           ac-source-variables
                           ac-source-symbols
                           ac-source-features
                           ac-source-abbrev
                           ac-source-words-in-same-mode-buffers
                           ac-source-dictionary))

;; Smarcase completion.
(setq ac-ignore-case 'smart)


(provide 'init-auto-completion)
