(defconst term:shell "zsh"
  "The path to the shell that should be run.")

(defvar term:previous-window-configuration nil
  "Holds the previous window configuration.")

(defvar term:current-term-buffer nil
  "Holds the current term buffer.")

(defun term:fullscreen ()
  (setq term:previous-window-configuration (current-window-configuration))
  (delete-other-windows)
  (if term:current-term-buffer
      (switch-to-buffer term:current-term-buffer)
    (setq term:current-term-buffer (ansi-term term:shell))))

(defun term:restore ()
  (set-window-configuration term:previous-window-configuration))

(defun term:kill ()
  (interactive)
  (when term:current-term-buffer
    (kill-buffer term:current-term-buffer)
    (setq term:current-term-buffer nil)))

(defun term:toggle ()
  (interactive)
  (if term:previous-window-configuration
      (progn
	(term:restore)
	(setq term:previous-window-configuration nil))
    (term:fullscreen)))


(provide 'init-term)
