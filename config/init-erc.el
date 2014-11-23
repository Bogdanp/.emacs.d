;; ERC
;; ~~~
;; Default config.
(setq erc-server "irc.freenode.net"
      erc-port 6667
      erc-nick "bogdanp"
      erc-user-full-name user-full-name)

;; Highlight these things in incoming messages.
(setq erc-keywords '("bogdanp"))

;; Autojoin these channels on freenode.
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#erc" "#haskell" "#python" "#scala" "#purescript")))

;; Behave like a "normal" IRC client.
(setq erc-kill-buffer-on-part t)
(setq erc-kill-queries-on-quit t)
(setq erc-kill-server-buffer-on-quit t)


(defun my-erc-mode-hook ()
  ;; Reset the margin in ERC so the command line doesn't get padded
  ;; unnecessarily.
  (setq-local scroll-margin 0))

(add-hook 'erc-mode-hook 'my-erc-mode-hook)


(provide 'init-erc)
