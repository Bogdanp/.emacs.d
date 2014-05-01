(add-hook 'prodigy-mode-hook 'evil-emacs-state)

(prodigy-define-service
  :name "LeadPages Server"
  :command (expand-file-name "~/Work/lead-pages/runserver")
  :cwd (expand-file-name "~/Work/lead-pages/")
  :tags '(work)
  :stop-signal 'sigterm
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LeadPages AWeber Server"
  :command (expand-file-name "~/Work/leadpages-integrations/AWeber/runserver")
  :cwd (expand-file-name "~/Work/leadpages-integrations/AWeber/")
  :tags '(work)
  :stop-signal 'sigterm
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "defn.io"
  :command (expand-file-name "~/sandbox/defn/dist/build/defn/defn")
  :cwd (expand-file-name "~/sandbox/defn/")
  :args `("-p" "8093" ,(expand-file-name "~/sandbox/defn/articles/"))
  :tags '(personal)
  :stop-signal 'sigterm
  :kill-process-buffer-on-stop t)

(provide 'init-prodigy)
