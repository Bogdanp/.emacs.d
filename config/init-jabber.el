;; Jabber
;; ~~~~~~
(setq ssl-program-name "gnutls-cli"
      ssl-program-arguments '("--insecure" "-p" service host)
      ssl-certificate-verification-policy 1)

(setq jabber-account-list nil)

;; HipChat
;; ~~~~~~~
(defvar hipchat-number "")
(defvar hipchat-nickname "")

(defun hipchat-join (room)
  "Join ROOM in HipChat."
  (interactive "sRoom name: ")
  (jabber-groupchat-join
   (jabber-read-account)
   (concat hipchat-number "_" room "@conf.hipchat.com")
   hipchat-nickname
   t))

(defun hipchat-mention (nickname)
  "Mention NICKNAME in HipChat."
  (interactive
    (list (jabber-muc-read-nickname jabber-group "Nickname: ")))
      (insert (concat "@\"" nickname "\" ")))


(provide 'init-jabber)
