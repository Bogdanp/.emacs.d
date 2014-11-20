(add-hook 'python-mode-hook
          (lambda ()
            (pretty-lambda-mode t)

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
  (let* ((current-line (s-trim (thing-at-point 'line)))
         (segments (split-string current-line " "))
         (filename (substring (nth 1 segments) 1 -2))
         (line (string-to-number (substring (nth 3 segments) 0 -1))))
    (find-file filename)
    (goto-line line)))

(defun python:eval-region (start end)
  "Evaluates simple Python code that's part of a region."
  (interactive "r")
  (save-excursion
    (let* ((prompt "# => ")
           (source (s-trim (buffer-substring-no-properties start end)))
           (interpreter (concat "from code import InteractiveConsole;"
                                "interpreter = InteractiveConsole();"
                                "code = '''%s'''.split('\\n');"
                                "[interpreter.runsource(line.strip()) for line in code];"))
           (code (format interpreter source))
           (result-lines (process-lines "python" "-c" code))
           (result (mapconcat #'identity result-lines (concat "\n" prompt)))
           (text (concat prompt result "\n")))
      (goto-char end)
      (insert text)
      (indent-region end (+ end (length text))))))


;; Jedi utils
;; ~~~~~~~~~~
(defun jedi:workon (path)
  (interactive "fVirtual env: ")
  (jedi:stop-server)
  (setq jedi:server-args
        `("--virtual-env" ,(expand-file-name path)))
  (jedi:install-server-block)
  (jedi:start-server)
  (jedi:setup))

(defun jedi:workon-default ()
  (interactive)
  (jedi:stop-server)
  (setq jedi:server-args nil)
  (jedi:start-server)
  (jedi:setup))


;; Testing
;; ~~~~~~~
(defun python:pytest-current-folder ()
  (interactive)
  (let* ((filename (buffer-file-name))
         (folder (f-dirname filename))
         (has-runner (f-exists? (f-join folder "runner.py"))))
    (if (not has-runner)
        (let* ((folder (f-dirname folder))
               (runner (f-join folder "runner.py")))
          (compile (string-join (list "python" runner ".") " ")))
        (compile "python runner.py .")))
  (switch-to-buffer-other-window "*compilation*"))


(defun python:pytest-current-file ()
  (interactive)
  (let* ((filename (buffer-file-name))
         (folder (f-dirname filename))
         (has-runner (f-exists? (f-join folder "runner.py"))))
    (if (not has-runner)
        (let* ((filename (substring filename (+ 1 (length folder))))
               (folder (f-dirname folder))
               (runner (f-join folder "runner.py")))
          (compile (string-join (list "python" runner filename) " ")))
        (compile (string-join (list "python runner.py" filename) " "))))
  (switch-to-buffer-other-window "*compilation*"))


(provide 'init-lang-python)
