;; Ensime
;; ~~~~~~
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(setq ensime-default-scala-version "2.11.2"
      ensime-default-java-flags '("-Xms256M" "-Xmx1G")
      ensime-sbt-command "activator")


(provide 'init-lang-scala)
