;; Misc
;; ~~~~
;; Paths to my org files.
(setq org:dir (expand-file-name "~/Dropbox/Documents/Personal"))
(setq org:main-file (concat org:dir "/Bogdan.org"))

;; PDF generation
;; ~~~~~~~~~~~~~~
;; Allow pdflatex to call external programs.
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Highlight code in exported PDFs.
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
           '(("frame" "lines")
             ("fontsize" "\\scriptsize")
             ("linenos" "")))

(add-to-list 'org-latex-packages-alist '("" "minted"))


;; Code blocks
;; ~~~~~~~~~~~
;; Highlight code in BEGIN_SRC-END_SRC blocks.
(setq org-src-fontify-natively t)

;; Allow these languages to be executed in org code blocks.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (sh . t)))


;; Org-capture
;; ~~~~~~~~~~~
;; Where to put captured stuff.
(setq org-default-notes-file org:main-file)

;; Capture templates.
(setq org-capture-templates
      '(("T" "Barebones TODO" entry (file+headline org:main-file "Tasks")
         "* TODO %?\n  %i")
        ("t" "TODO" entry (file+headline org:main-file "Tasks")
         "* TODO %?\n  :PROPERTIES:\n  :Created: %u\n  :Source:  %a\n  :END:")
        ("i" "Idea" entry (file+headline org:main-file "Ideas")
         "* %?\n  :PROPERTIES:\n  :Created: %u\n  :END:")))


;; Agenda
;; ~~~~~~
;; Set up path to agenda files.
(setq org:agenda-files-path org:dir)

(when (file-exists-p org:agenda-files-path)
  (setq org-agenda-files `(,org:agenda-files-path)))


;; TODOs
;; ~~~~~
;; Log the closing time of TODO items.
(setq org-log-done 'time)

;; Better todo states.
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))


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
