;; Scala Language
;; ~~~~~~~~~~~~~~
(use-package scala-mode2
  :defer t
  :ensure t)


;; sbt-mode
;; ~~~~~~~~
(use-package sbt-mode
  :defer t
  :ensure t)


;; ENSIME
;; ~~~~~~
(use-package ensime
  :bind (("C-c C-." . ensime-edit-definition-other-window)
         ("C-c ." . ensime-edit-definition)
         ("C-c ," . ensime-pop-find-definition-stack))
  :defer t
  :ensure t
  :config
  (progn
    (setq ensime-default-scala-version "2.11.2"
          ensime-default-java-flags '("-Xms256M" "-Xmx1G")
          ensime-sbt-command "activator")

    (defun my-ensime-mode-hook ()
      ;; Disable auto-complete-mode since ensime uses Company mode
      ;; now.
      (auto-complete-mode -1))

    (add-hook 'ensime-mode-hook #'my-ensime-mode-hook)
    (add-hook 'scala-mode-hook #'ensime-scala-mode-hook)))



(provide 'init-lang-scala)
