(defun elisp:byte-compile-configs ()
  "Byte-compiles everything inside the .emacs.d folder recursively."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun elisp:remove-elc-on-save ()
  "Deletes the .elc file for any elisp file on save."
  (add-hook 'after-save-hook
	    (lambda ()
	      (let ((filename (concat buffer-file-name "c")))
		(when (file-exists-p filename)
		  (delete-file filename)))
	      t)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'pretty-lambda-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp:remove-elc-on-save)


(provide 'init-lang-elisp)
