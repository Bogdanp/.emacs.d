;;; init-term.el --- term goodies
;;; Commentary:
;;; Code:
;;; Zipper
(with-no-warnings
  (require 'cl))

(cl-defstruct zipper lhs curr rhs)

(defun zipper-append (zipper x)
  "Append to ZIPPER the value of X."
  (setf (zipper-rhs zipper)
        (reverse (cons x (reverse (zipper-rhs zipper))))))

(defun zipper-drop (zipper)
  "Drop the current element from ZIPPER."
  (setf (zipper-curr zipper) nil)
  (zipper-next zipper))

(defun zipper-beginning (zipper)
  "Goto the beginning of ZIPPER."
  (setf (zipper-rhs zipper)
        (append (reverse (cons (zipper-curr zipper)
                               (zipper-lhs zipper)))
                (zipper-rhs zipper)))
  (setf (zipper-curr zipper) nil)
  (setf (zipper-lhs zipper) nil))

(defun zipper-end (zipper)
  "Goto the end of ZIPPER."
  (setf (zipper-lhs zipper)
        (append (reverse (cons (zipper-curr zipper)
                               (zipper-rhs zipper)))
                (zipper-lhs zipper)))
  (setf (zipper-rhs zipper) nil)
  (setf (zipper-curr zipper)
        (car (zipper-lhs zipper)))
  (setf (zipper-lhs zipper)
        (cdr (zipper-lhs zipper))))

(defmacro defmover (name f g)
  "Define a zipper modifier function called NAME.

F is where data gets moved to.
G is where data gets moved from."
  `(defun ,name (zipper)
     (when (funcall ,f zipper)
       (let ((x  (car (funcall ,f zipper)))
             (xs (cdr (funcall ,f zipper))))

         (when (zipper-curr zipper)
           (setf (,(cadr g) zipper)
                 (cons (zipper-curr zipper)
                       (funcall ,g zipper))))

         (setf (zipper-curr zipper) x)
         (setf (,(cadr f) zipper) xs)))
     (zipper-curr zipper)))

(defmover zipper-next #'zipper-rhs #'zipper-lhs)
(defmover zipper-prev #'zipper-lhs #'zipper-rhs)

;;; Term
(require 'ansi-color)

(defconst bp-term-shell "zsh"
  "The path to the shell that should be run.")

(defvar bp-term-previous-window-configuration nil
  "Holds the previous window configuration.")

(defvar bp-term-current-term-buffer nil
  "Holds the current term buffer.")

(defvar bp-term-terms
  (make-zipper :lhs  nil
               :rhs  nil
               :curr nil)
  "A zipper for all of the existing terms.")

(defun bp-maybe-switch-to-buffer (buffer)
  "Switch to BUFFER iff it is non-nil."
  (when buffer
    (switch-to-buffer buffer)))

(defun bp-term-add ()
  "Add a new terminal and jump to it."
  (interactive)
  (zipper-end bp-term-terms)
  (zipper-append bp-term-terms (ansi-term bp-term-shell))
  (bp-term-next))

(defun bp-term-kill ()
  "Kill the current terminal."
  (interactive)
  (when (>= (length (zipper-rhs bp-term-terms)) 1)
    (let ((buffer (zipper-drop bp-term-terms)))
      (kill-buffer bp-term-current-term-buffer)
      (setq bp-term-current-term-buffer buffer)
      (bp-maybe-switch-to-buffer buffer))))

(defun bp-term-next ()
  "Goto the next terminal in the zipper."
  (interactive)
  (let ((buffer (zipper-next bp-term-terms)))
    (setq bp-term-current-term-buffer buffer)
    (bp-maybe-switch-to-buffer buffer)))

(defun bp-term-prev ()
  "Goto the previous terminal in the zipper."
  (interactive)
  (let ((buffer (zipper-prev bp-term-terms)))
    (setq bp-term-current-term-buffer buffer)
    (bp-maybe-switch-to-buffer buffer)))

(defun bp-term-fullscreen ()
  "Make the term fullscreen."
  (setq bp-term-previous-window-configuration (current-window-configuration))
  (delete-other-windows)
  (if bp-term-current-term-buffer
      (bp-maybe-switch-to-buffer bp-term-current-term-buffer)
    (bp-term-add)
    (setq bp-term-current-term-buffer (zipper-curr bp-term-terms))))

(defun bp-term-toggle ()
  "Toggle between the current window config and a terminal."
  (interactive)
  (if bp-term-previous-window-configuration
      (progn
        (set-window-configuration bp-term-previous-window-configuration)
	(setq bp-term-previous-window-configuration nil))
    (bp-term-fullscreen)))

(defun bp-term-clipboard-paste ()
  "Paste the contents of the clipboard into the current term."
  (interactive)
  (term-send-raw-string (get-clipboard-value)))


;;; Server
(defun my-server-visit-hook-for-term ()
  "Most of the time I call `emacsclient' I'll be toggled-into `bp-term-**'.

I don't want calling `emacsclient' to break that configuration so this
hook works around that by toggling out of that configuration before
switching to the new buffer."
  (let ((buffer (current-buffer)))
    (when bp-term-previous-window-configuration
      (bp-term-toggle)
      (switch-to-buffer buffer))))

(add-hook 'server-visit-hook #'my-server-visit-hook-for-term)


(provide 'init-term)
;;; init-term.el ends here
