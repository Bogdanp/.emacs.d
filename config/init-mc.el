;; Make MC work with EVIL mode
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defvar evil:previous-state nil)

(defun evil:switch-to-emacs-state ()
  (when (and (bound-and-true-p evil-mode)
             (not (eq evil-state 'emacs)))
    (setq evil:previous-state evil-state)
    (evil-emacs-state)))

(defun evil:back-to-previous-state ()
  (when evil:previous-state
    (unwind-protect
        (case evil:previous-state
          ((normal visual insert) (evil-force-normal-state))
          (t (message "Don't know how to handle previous state: %S"
                      evil:previous-state)))
      (setq evil:previous-state nil))))

(add-hook 'multiple-cursors-mode-enabled-hook
          'evil:switch-to-emacs-state)
(add-hook 'multiple-cursors-mode-disabled-hook
          'evil:back-to-previous-state)


(provide 'init-mc)
