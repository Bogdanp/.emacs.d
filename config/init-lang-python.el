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
;; Set up py-test.
(add-to-list 'load-path (expand-file-name "~/sandbox/py-test"))

(require 'py-test)

;; LeadPages project.
(py-test/define-project
 :name "LeadPages"
 :base-directory (expand-file-name "~/Work/lead-pages/")
 :test-runner (expand-file-name "~/Work/lead-pages/tests/unit/runner.py")
 :working-directory (expand-file-name "~/Work/lead-pages/tests/unit/"))


(provide 'init-lang-python)
