;; Zipper
;; ~~~~~~
;; TODO: Move this someplace more appropriate.
(cl-defstruct zipper lhs curr rhs)

(defun zipper-append (zipper x)
  (setf (zipper-rhs zipper)
        (reverse (cons x (reverse (zipper-rhs zipper))))))

(defun zipper-drop (zipper)
  (setf (zipper-curr zipper) nil)
  (zipper-next zipper))

(defun zipper-curr-list (zipper)
  (if (zipper-curr zipper)
      `(,(zipper-curr zipper))
      nil))

(defun zipper-beginning (zipper)
  (setf (zipper-rhs zipper)
        (append (zipper-lhs zipper)
                (zipper-curr-list zipper)
                (zipper-rhs zipper)))
  (setf (zipper-curr zipper) nil)
  (setf (zipper-lhs zipper) nil))

(defun zipper-end (zipper)
  (setf (zipper-lhs zipper)
        (reverse (append (zipper-lhs zipper)
                         (zipper-curr-list zipper)
                         (zipper-rhs zipper))))
  (setf (zipper-rhs zipper) nil)
  (setf (zipper-curr zipper) nil))

(defmacro defmover (name f g)
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


;; Term
;; ~~~~
(defconst term:shell "zsh"
  "The path to the shell that should be run.")

(defvar term:previous-window-configuration nil
  "Holds the previous window configuration.")

(defvar term:current-term-buffer nil
  "Holds the current term buffer.")

(defvar term:terms
  (make-zipper :lhs  nil
               :rhs  nil
               :curr nil)
  "A zipper for all of the existing terms.")

(defun term:add ()
  (interactive)
  (zipper-end term:terms)
  (zipper-append term:terms (ansi-term term:shell))
  (term:next))

(defun term:kill ()
  (interactive)
  (let ((buffer (zipper-drop term:terms)))
    (kill-buffer term:current-term-buffer)
    (setq term:current-term-buffer buffer)
    (switch-to-buffer buffer)))

(defun term:next ()
  (interactive)
  (let ((buffer (zipper-next term:terms)))
    (setq term:current-term-buffer buffer)
    (switch-to-buffer buffer)))

(defun term:prev ()
  (interactive)
  (let ((buffer (zipper-prev term:terms)))
    (setq term:current-term-buffer buffer)
    (switch-to-buffer buffer)))

(defun term:fullscreen ()
  (setq term:previous-window-configuration (current-window-configuration))
  (delete-other-windows)
  (if term:current-term-buffer
      (switch-to-buffer term:current-term-buffer)
      (progn
        (term:add)
        (setq term:current-term-buffer (zipper-curr term:terms)))))

(defun term:restore ()
  (set-window-configuration term:previous-window-configuration))

(defun term:toggle ()
  (interactive)
  (if term:previous-window-configuration
      (progn
	(term:restore)
	(setq term:previous-window-configuration nil))
    (term:fullscreen)))


(provide 'init-term)
