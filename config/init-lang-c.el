;; C Language
;; ~~~~~~~~~~
(setq c-default-style "bsd"
      c-basic-offset 4)


;; Hooks
;; ~~~~~
;; Setup indentation.
(defun my-c-mode-hook ()
  (c-set-offset 'arglist-intro '+)

  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|NOTE\\)\\(([^)]+)\\)?:"
                             1 font-lock-warning-face t))))

(add-hook 'c-mode-hook 'my-c-mode-hook)


(provide 'init-lang-c)
