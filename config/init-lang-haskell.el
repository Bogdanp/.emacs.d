;; Haskell Language
;; ~~~~~~~~~~~~~~~~
(setq haskell-process-common-args '("--ghc-option=-ferror-spans"
                                    "--ghc-option=-fno-warn-name-shadowing"
                                    "--ghc-option=-fno-warn-orphans"))

(custom-set-variables
 ;; Haskell Process
 '(haskell-process-type 'cabal-repl)
 '(haskell-process-args-cabal-repl `(,@haskell-process-common-args))
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-use-presentation-mode t)
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
    (remove-if (lambda (c) (string= "" c)) cs)))

(ac-define-source haskell
  '((candidates . (ac-haskell-candidates ac-prefix))))


;; Hooks
;; ~~~~~
;; Haskell
(defun my-haskell-mode-hook ()
  (ghc-init)
  (turn-on-haskell-doc-mode)
  (structured-haskell-mode t)

  (add-to-list 'ac-sources 'ac-source-haskell)

  (setq-local ac-auto-start 2)
  (setq-local indent-line-function #'indent-relative))

(add-hook 'haskell-mode-hook #'my-haskell-mode-hook)
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

;; Setup purty symbols.
(defun my-haskell-mode-prettify-symbols-hook ()
  (setf prettify-symbols-alist
        (append '(;;; Syntax
                  ("forall" . ?∀)
                  ("::"     . ?∷)
                  ("\\"     . ?λ)
                  ("->"     . ?→)
                  ("=>"     . ?⇒)

                  ;;; Functions
                  ;; Prefix
                  ("not"         . ?¬)
                  ("and"         . ?∧)
                  ("or"          . ?∨)
                  ;; Infix
                  (">>="         . ?↪)
                  ("=<<"         . ?↩)
                  ("."           . ?∘)
                  ("*"           . ?×)
                  (":="          . ?≔)
                  ("|-"          . ?⊢)
                  ("-|"          . ?⊣)
                  ("<="          . ?≤)
                  (">="          . ?≥)
                  ("=="          . ?≡)
                  ("/="          . ?≠)
                  ("&&"          . ?∧)
                  ("||"          . ?∨)
                  ("!!"          . ?‼)
                  ("`div`"       . ?÷)
                  ("`elem`"      . ?∈)
                  ("`notElem`"   . ?∉)
                  ("`union`"     . ?∪)
                  ("`intersect`" . ?∩))
                prettify-symbols-alist)))

(add-hook 'haskell-mode-hook #'prettify-symbols-mode)
(add-hook 'haskell-mode-hook #'my-haskell-mode-prettify-symbols-hook)


(provide 'init-lang-haskell)
