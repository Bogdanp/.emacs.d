;; Settings
;; ~~~~~~~~
(custom-set-variables
 ;; Haskell Process
 '(haskell-process-type 'cabal-repl)
 '(haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-use-presentation-mode t)
 '(haskell-process-do-cabal-format-string ":!cd %s && unset GHC_PACKAGE_PATH && %s")
 '(haskell-process-show-debug-tips nil)

 ;; Haskell Interactive
 '(haskell-interactive-mode-do-fast-keys t)
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-mode-include-file-name nil)

 ;; Misc
 '(haskell-stylish-on-save t)
 '(haskell-notify-p t)
 '(haskell-tags-on-save t)

 ;; SHM
 '(shm-auto-insert-skeletons t)
 '(shm-auto-insert-bangs t)
 '(shm-use-hdevtools nil)
 '(shm-use-presentation-mode t))


;; Auto Complete
;; ~~~~~~~~~~~~~
(defun ac-haskell-candidates (prefix)
  (let ((cs (haskell-process-get-repl-completions (haskell-process) prefix)))
    (remove-if #'(lambda (c) (string= "" c)) cs)))

(ac-define-source haskell
  '((candidates . (ac-haskell-candidates ac-prefix))))


;; Hooks
;; ~~~~~
;; Haskell
(add-hook 'haskell-mode-hook #'(lambda ()
                                 (ghc-init)
                                 (turn-on-haskell-doc-mode)
                                 (structured-haskell-mode t)

                                 (add-to-list 'ac-sources 'ac-source-haskell)

                                 (setq-local ac-auto-start 2)
                                 (setq-local indent-line-function #'indent-relative)))

;; Flycheck
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)


(provide 'init-lang-haskell)
