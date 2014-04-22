(require 'init-el-get)

(add-hook 'ghc-mode-hook (lambda ()
                           (setq ac-auto-start 4)))

;; Don't warn about name shadowing.
(setq ghc-ghc-options '("-fno-warn-hi-shadowing"
                        "-fno-warn-name-shadowing"))

(provide 'init-lang-haskell)
