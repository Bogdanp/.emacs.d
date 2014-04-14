(add-hook 'prodigy-mode-hook 'evil-emacs-state)

(prodigy-define-service
  :name "LeadPages Server"
  :command "/Users/bogdan/Work/lead-pages/runserver"
  :cwd "/Users/bogdan/Work/lead-pages/"
  :tags '(work)
  :stop-signal 'sigterm
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LeadPages AWeber Server"
  :command "/Users/bogdan/Work/leadpages-integrations/AWeber/runserver"
  :cwd "/Users/bogdan/Work/leadpages-integrations/AWeber/"
  :tags '(work)
  :stop-signal 'sigterm
  :kill-process-buffer-on-stop t)

(provide 'init-prodigy)
