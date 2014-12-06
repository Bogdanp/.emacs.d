;; Ido
;; ~~~
(ido-mode t)
(ido-ubiquitous-mode)
(ido-vertical-mode)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-ignore-extensions t)


;; Smex
;; ~~~~
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)


(provide 'init-ido)
