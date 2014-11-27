;; Misc
;; ~~~~
;; Paths to my org files.
(setq bp-org-dir (expand-file-name "~/Dropbox/Documents/Personal"))
(setq bp-org-main-file (concat bp-org-dir "/Bogdan.org"))

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
   (clojure . t)
   (sh . t)))

;; Use cider instead of slime to evaluate clojure code.
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)


;; Org-capture
;; ~~~~~~~~~~~
;; Where to put captured stuff.
(setq org-default-notes-file bp-org-main-file)

;; Capture templates.
(setq org-capture-templates
      '(("T" "Barebones TODO" entry (file+headline bp-org-main-file "Tasks")
         "* TODO %?\n  %i")
        ("t" "TODO" entry (file+headline bp-org-main-file "Tasks")
         "* TODO %?\n  :PROPERTIES:\n  :Created: %u\n  :Source:  %a\n  :END:")
        ("i" "Idea" entry (file+headline bp-org-main-file "Ideas")
         "* %?\n  :PROPERTIES:\n  :Created: %u\n  :END:")
        ("m" "LeadPages Meeting" entry (file+olp bp-org-main-file "LeadPages" "Meetings")
         "* TODO %?\n  :PROPERTIES:\n  :Created: %u\n  :END:")
        ("n" "Note" entry (file+headline bp-org-main-file "Notes")
         "* %?\n  :PROPERTIES:\n  :Created: %u\n  :END:")))


;; Agenda
;; ~~~~~~
;; Set up path to agenda files.
(setq bp-org-agenda-files-path bp-org-dir)

(when (file-exists-p bp-org-agenda-files-path)
  (setq org-agenda-files `(,bp-org-agenda-files-path)))


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
(defun bp-org-agenda-to-appt ()
  "Erase all current reminders and rebuild the list from the current
agenda."
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; Plz don't ruin my window setup, org-agenda.
(setq org-agenda-window-setup 'current-window)

;; Display appointment info in the modeline.
(setq appt-display-mode-line t)

;; Rebuild reminders each time the agenda is displayed.
(add-hook 'org-finalize-agenda-hook #'bp-org-agenda-to-appt 'append)

;; Activate appointments.
(appt-activate t)

;; Reset appointments 1 minute after midnight.
(run-at-time "24:01" nil #'bp-org-agenda-to-appt)

;; Setup appointments at startup.
(bp-org-agenda-to-appt)


;; Archiving
;; ~~~~~~~~~
(defun bp-org-level-of-heading-at-point ()
  "Returns the level of the headline at point."
  (length (car (split-string (thing-at-point 'line t) " "))))

(defun bp-org-archive-task-at-point ()
  "Moves the task at point into the first heading of its parent (which,
by convention, should be an Archive heading)."
  (interactive)
  (save-excursion
    (let ((start-level (bp-org-level-of-heading-at-point)))
      (org-cut-subtree)

      ;; Cutting the subtree might place us on a different
      ;; level. Account for those cases.
      (let ((current-level (bp-org-level-of-heading-at-point)))
        (if (< current-level start-level)
            (progn
              (org-goto-sibling 'previous)
              (dotimes (number (- start-level current-level 1))
                (org-end-of-subtree)
                (org-goto-first-child)))
          (outline-up-heading (+ 1 (- current-level start-level)))))

      ;; TODO: Turn this into a heading search?
      (org-goto-first-child)

      (let ((archive-level (bp-org-level-of-heading-at-point)))
        (next-line)
        (org-paste-subtree (+ 1 archive-level))))))


(provide 'init-org)
