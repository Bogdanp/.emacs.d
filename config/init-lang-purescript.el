;; Setup
;; ~~~~~
;; Load purescript from the local repo.
(add-to-list 'load-path "~/sandbox/purescript-mode/")
(require 'purescript-mode-autoloads)


;; Setup purscheck
;; ~~~~~~~~~~~~~~~
(add-to-list 'load-path "~/sandbox/purscheck/")
(require 'purscheck)

;; Turn on flycheck inside of purescript buffers.
(add-hook 'purescript-mode-hook 'flycheck-mode)


(provide 'init-lang-purescript)
