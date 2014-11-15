(defun move-current-frame-to-1st-monitor ()
  (interactive)
  (set-frame-size (selected-frame) 205 53)
  (set-frame-position (selected-frame) 10 32))

(defun move-current-frame-to-2nd-monitor ()
  (interactive)
  (set-frame-size (selected-frame) 235 63)
  (set-frame-position (selected-frame) 1690 32))

(defun create-frame-on-1st-monitor ()
  (interactive)
  (new-frame)
  (move-current-frame-to-1st-monitor))

(defun create-frame-on-2nd-monitor ()
  (interactive)
  (new-frame)
  (move-current-frame-to-2nd-monitor))


(provide 'init-frame)
