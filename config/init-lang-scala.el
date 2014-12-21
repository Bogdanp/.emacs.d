;; Scala Language
;; ~~~~~~~~~~~~~~
(setq ensime-default-scala-version "2.11.2"
      ensime-default-java-flags '("-Xms256M" "-Xmx1G")
      ensime-sbt-command "activator")


;; Hooks
;; ~~~~~
(defun my-ensime-mode-hook ()
  ;; Disable auto-complete-mode since ensime uses Company mode now.
  (auto-complete-mode -1))

(add-hook 'ensime-mode-hook #'my-ensime-mode-hook)
(add-hook 'scala-mode-hook #'ensime-scala-mode-hook)


(provide 'init-lang-scala)
