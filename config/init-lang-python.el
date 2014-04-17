(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (setq-local jedi:complete-on-dot t)

            ;; Don't start automatically (causes SERIOUS performance issues on
            ;; large Python files (> 1k LOC)).
            (setq-local ac-auto-start nil)))


;; Utility functions
;; ~~~~~~~~~~~~~~~~~
(defun python:trace-find-file-at-point ()
  "Opens up the file at the point when looking at a Python stack
trace."
  (interactive)
  (let* ((segments (split-string (s-trim (thing-at-point 'line)) " "))
         (filename (substring (nth 1 segments) 1 -2))
         (line (substring (nth 3 segments) 0 -1)))
    (switch-to-buffer
     (find-file filename)
     (goto-line (string-to-number line)))))


(provide 'init-lang-python)
