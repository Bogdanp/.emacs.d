(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (setq-local jedi:complete-on-dot t)

            ;; Don't start automatically (causes SERIOUS performance issues on
            ;; large Python files (> 1k LOC)).
            (setq-local ac-auto-start nil)))

(provide 'init-lang-python)
