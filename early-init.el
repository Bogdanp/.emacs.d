;;; early-init.el -- set up env vars before emacs launches
;;; Commentary:
;;; Code:
(when (equal system-configuration "aarch64-apple-darwin22.6.0")
  (setenv "LIBRARY_PATH"
          (string-join
           '("/opt/local/lib/gcc12/"
             "/opt/local/lib/gcc12/gcc/arm64-apple-darwin22/12.3.0/"
             "/opt/local/lib")
           ":")))
;;; early-init.el ends here
