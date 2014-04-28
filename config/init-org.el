;; Highlight code in BEGIN_SRC-END_SRC blocks.
(setq org-src-fontify-natively t)

;; Set up path to agenda files.
(setq org-agenda-files `(,(expand-file-name "~/Documents/Org/")))

;; Log the closing time of TODO items.
(setq org-log-done 'time)


;; Reminders
;; ~~~~~~~~~
;; Code below mostly stolen from http://doc.norang.ca/org-mode.html#Reminders
(defun org/agenda-to-appt ()
  "Erase all current reminders and rebuild the list from the current
agenda."
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; Display appointment info in the modeline.
(setq appt-display-mode-line t)

;; Rebuild reminders each time the agenda is displayed.
(add-hook 'org-finalize-agenda-hook #'org/agenda-to-appt 'append)

;; Activate appointments.
(appt-activate t)

;; Reset appointments 1 minute after midnight.
(run-at-time "24:01" nil #'org/agenda-to-appt)

;; Setup appointments at startup.
(org/agenda-to-appt)


(provide 'init-org)
