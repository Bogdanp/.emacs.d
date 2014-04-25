(add-to-list 'load-path "~/.emacs.d/packages/ensime/src/main/elisp/")

(require 'ensime)

(defun scala:test-toggle-path (fp)
  "Test whether FP is one of '(mvn-source mvn-test play-source
play-test) and transform it into its pair (source to test and
vice-versa).

  (scala:test-toggle-path
    \"/home/Users/bogdan/sandbox/foo/src/main/scala/Main.scala\")
    => \"/home/Users/bogdan/sandbox/foo/src/test/scala/MainSpec.scala\"

  (scala:test-toggle-path
    \"/home/Users/bogdan/sandbox/foo/src/test/scala/MainSpec.scala\")
    => \"/home/Users/bogdan/sandbox/foo/src/main/scala/Main.scala\""

  (cl-flet ((m (s) (string-match-p s fp))

            (rg (s r) (replace-regexp-in-string s r fp))
            (rs (s r) (replace-regexp-in-string "Spec\.scala$" ".scala" (rg s r)))
            (rt (s r) (replace-regexp-in-string "\.scala$" "Spec.scala" (rg s r)))

            (mvn-source-p  () (m "/src/main/scala/.*.scala"))
            (mvn-test-p    () (m "/src/test/scala/.*.scala"))

            (play-source-p () (m "/app/.*.scala"))
            (play-test-p   () (m "/test/.*.scala"))

            (mvn-test-to-source  () (rs "/src/test/" "/src/main/"))
            (mvn-source-to-test  () (rt "/src/main/" "/src/test/"))

            (play-test-to-source () (rs "/test/" "/app/"))
            (play-source-to-test () (rt "/app/" "/test/")))

    (cond ((mvn-source-p) (mvn-source-to-test))
          ((mvn-test-p) (mvn-test-to-source))
          ((play-source-p) (play-source-to-test))
          ((play-test-p) (play-test-to-source))
          (t nil))))

(defun scala:toggle-test ()
  "Toggle between a Scala spec file and its implementation."
  (interactive)
  (let* ((fp (buffer-file-name))
         (np (scala:test-toggle-path fp)))
    (if np
        (progn
          (mkdir (file-name-directory fp) t)
          (find-file np))
      (message "Could not determine correct path"))))

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c t") 'scala:toggle-test)))

(provide 'init-lang-scala)
