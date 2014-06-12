;; Set up Ensime
;; ~~~~~~~~~~~~~
(add-to-list 'load-path (expand-file-name "~/sandbox/ensime/dist/elisp"))
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


(provide 'init-lang-scala)
