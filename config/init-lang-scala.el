;; Scala Language
;; ~~~~~~~~~~~~~~
(setq ensime-default-scala-version "2.11.2"
      ensime-default-java-flags '("-Xms256M" "-Xmx1G")
      ensime-sbt-command "activator")


;; Hooks
;; ~~~~~
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


(provide 'init-lang-scala)
