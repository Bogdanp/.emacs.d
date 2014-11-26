;; Zipper
;; ~~~~~~
(cl-defstruct zipper lhs curr rhs)

(defun zipper-append (zipper x)
  (setf (zipper-rhs zipper)
        (reverse (cons x (reverse (zipper-rhs zipper))))))

(defun zipper-drop (zipper)
  (setf (zipper-curr zipper) nil)
  (zipper-next zipper))

(defun zipper-beginning (zipper)
  (setf (zipper-rhs zipper)
        (append (reverse (cons (zipper-curr zipper)
                               (zipper-lhs zipper)))
                (zipper-rhs zipper)))
  (setf (zipper-curr zipper) nil)
  (setf (zipper-lhs zipper) nil))

(defun zipper-end (zipper)
  (setf (zipper-lhs zipper)
        (append (reverse (cons (zipper-curr zipper)
                               (zipper-rhs zipper)))
                (zipper-lhs zipper)))
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

(defun bp-term-add ()
  (interactive)
  (zipper-end bp-term-terms)
  (zipper-append bp-term-terms (ansi-term bp-term-shell))
  (bp-term-next))

(defun bp-term-kill ()
  (interactive)
  (let ((buffer (zipper-drop bp-term-terms)))
    (kill-buffer bp-term-current-term-buffer)
    (setq bp-term-current-term-buffer buffer)
    (switch-to-buffer buffer)))

(defun bp-term-next ()
  (interactive)
  (let ((buffer (zipper-next bp-term-terms)))
    (setq bp-term-current-term-buffer buffer)
    (switch-to-buffer buffer)))

(defun bp-term-prev ()
  (interactive)
  (let ((buffer (zipper-prev bp-term-terms)))
    (setq bp-term-current-term-buffer buffer)
    (switch-to-buffer buffer)))

(defun bp-term-fullscreen ()
  (setq bp-term-previous-window-configuration (current-window-configuration))
  (delete-other-windows)
  (if bp-term-current-term-buffer
      (switch-to-buffer bp-term-current-term-buffer)
    (progn
      (bp-term-add)
      (setq bp-term-current-term-buffer (zipper-curr bp-term-terms)))))

(defun bp-term-restore ()
  (set-window-configuration bp-term-previous-window-configuration))

(defun bp-term-toggle ()
  (interactive)
  (if bp-term-previous-window-configuration
      (progn
	(bp-term-restore)
	(setq bp-term-previous-window-configuration nil))
    (bp-term-fullscreen)))


;; Misc
;; ~~~~
(defun my-term-mode-hook ()
  ;; Reset the margin in term mode so the command line doesn't get
  ;; padded unnecessarily.
  (setq-local scroll-margin 0))

(add-hook 'term-mode-hook 'my-term-mode-hook)


(provide 'init-term)
