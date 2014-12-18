;; Python Language
;; ~~~~~~~~~~~~~~~
(defun bp-python-trace-find-file-at-point ()
  "Opens up the file at the point when looking at a Python stack
trace."
  (interactive)
  (let* ((current-line (s-trim (thing-at-point 'line)))
         (segments (split-string current-line " "))
         (filename (substring (nth 1 segments) 1 -2))
         (line (string-to-number (substring (nth 3 segments) 0 -1))))
    (find-file filename)
    (goto-line line)))

(defun bp-python-eval-region (start end)
  "Evaluates a region of Python code."
  (interactive "r")
  (save-excursion
    (let* ((prompt "# => ")
           (source (s-trim (buffer-substring-no-properties start end)))
           (filename (make-temp-file "python-eval-region"))
           (_ (f-write source 'utf-8 filename))
           (result-lines (process-lines "python" filename))
           (result (mapconcat #'identity result-lines (concat "\n" prompt)))
           (text (concat prompt result "\n")))
      (f-delete filename)
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
;; Purty mode-line.
(setq py-test-*mode-line-face-shenanigans-on* t)
(setq py-test-*mode-line-face-shenanigans-timer* "0.5 sec")

;; py-test projects ahoy:
(py-test-define-project
 :name "LeadPages"
 :python-command "python"
 :base-directory (expand-file-name "~/Work/lead-pages/")
 :test-runner (expand-file-name "~/Work/lead-pages/tests/unit/runner.py")
 :test-runner-arguments '("-sv")
 :working-directory (expand-file-name "~/Work/lead-pages/tests/unit/"))


;; Hooks
;; ~~~~~
(defun my-python-mode-hook ()
  (pretty-lambda-mode t)

  (jedi:setup)
  (setq-local jedi:complete-on-dot t)
  (setq-local jedi:tooltip-method nil)

  (yas-minor-mode)

  ;; Don't start automatically (causes SERIOUS performance issues on
  ;; large Python files (> 1k LOC)).
  (setq-local ac-auto-start nil))

(add-hook 'python-mode-hook 'my-python-mode-hook)


(provide 'init-lang-python)
