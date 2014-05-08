(add-hook 'prodigy-mode-hook 'evil-emacs-state)

(setq prodigy:screenshot-service-env
      `(("PHANTOMJS_BIN_PATH" "/usr/local/bin/phantomjs")
        ("PHANTOMJS_CAPTURE_PATH" ,(expand-file-name "~/Work/screenshot-service/phantomjs/capture.js"))))

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
  :name "LeadPages Screenshot Service"
  :command (expand-file-name "~/Work/screenshot-service/venv/bin/python")
  :cwd (expand-file-name "~/Work/screenshot-service/")
  :args `(,(expand-file-name "~/Work/screenshot-service/service/app.py") "5000")
  :env  prodigy:screenshot-service-env
  :tags '(work)
  :stop-signal 'sigterm
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LeadPages Screenshot Service Consumer"
  :command (expand-file-name "~/Work/screenshot-service/venv/bin/python")
  :cwd (expand-file-name "~/Work/screenshot-service/consumer/")
  :args `(,(expand-file-name "~/Work/screenshot-service/consumer/consumer.py"))
  :env prodigy:screenshot-service-env
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

(prodigy-define-service
  :name "beanstalkd"
  :command "beanstalkd"
  :tags '(personal work)
  :stop-signal 'sigterm
  :kill-process-buffer-on-stop t)


(provide 'init-prodigy)
