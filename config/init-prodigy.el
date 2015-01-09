(setq bp-prodigy-screenshot-service-env
      `(("PHANTOMJS_BIN_PATH" "/usr/local/bin/phantomjs")
        ("PHANTOMJS_CAPTURE_PATH" ,(expand-file-name "~/Work/screenshot-service/phantomjs/capture.js"))))

(defun bp-prodigy-start-beanstalk& (k)
  (let ((beanstalkd (prodigy-find-service "beanstalkd")))
    (if (prodigy-service-started-p beanstalkd)
        (funcall k)
        (prodigy-start-service beanstalkd k))))

(defun bp-prodigy-toggle-compilation-mode ()
  (interactive)
  (if (eq major-mode 'compilation-mode)
      (prodigy-view-mode)
    (compilation-mode))
  (if (fboundp #'my-prodigy-view-mode-hook)
      (my-prodigy-view-mode-hook))
  (end-of-buffer))

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
  :env  bp-prodigy-screenshot-service-env
  :tags '(work)
  :stop-signal 'sigterm
  :kill-process-buffer-on-stop t
  :init-async #'bp-prodigy-start-beanstalk&)

(prodigy-define-service
  :name "LeadPages Screenshot Service Consumer"
  :command (expand-file-name "~/Work/screenshot-service/venv/bin/python")
  :cwd (expand-file-name "~/Work/screenshot-service/consumer/")
  :args `(,(expand-file-name "~/Work/screenshot-service/consumer/consumer.py"))
  :env bp-prodigy-screenshot-service-env
  :tags '(work)
  :stop-signal 'sigterm
  :kill-process-buffer-on-stop t
  :init-async #'bp-prodigy-start-beanstalk&)

(prodigy-define-service
  :name "beanstalkd"
  :command "beanstalkd"
  :tags '(personal work)
  :stop-signal 'sigterm
  :kill-process-buffer-on-stop t)


(provide 'init-prodigy)
