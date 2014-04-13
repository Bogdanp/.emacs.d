(add-hook 'python-mode-hook 'jedi:setup)

(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)

(provide 'init-jedi)
