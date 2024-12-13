(setq org-agenda-files '("~/.local/share/org/"))
(setq org-default-notes-file "~/.local/share/org/notes.org")

(defun org-agenda-todo-next ()
    "Org agenda todo next cycle"
    (interactive)
    (org-call-with-arg 'org-agenda-todo 'right))

(defun org-agenda-todo-prev ()
    "Org agenda todo next cycle"
    (interactive)
    (org-call-with-arg 'org-agenda-todo 'left))

(setq org-priority-faces
      '((?A . (:foreground "red" :weight bold))
        (?B . (:foreground "orange"))
        (?C . (:foreground "yellow"))))

(setq org-todo-keywords
      '((sequence "BACKLOG(b)" "WEEKLY(w)" "WAITING(h)" "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d)")))

(setq org-todo-keyword-faces
      '(("BACKLOG" . (:foreground "gray" :weight bold))
        ("WEEKLY" . (:foreground "purple" :weight bold))
        ("TODO" . (:foreground "white" :weight bold))
        ("NEXT" . (:foreground "yellow" :weight bold))
        ("IN-PROGRESS" . (:foreground "orange" :weight bold))
        ("WAITING" . (:foreground "blue" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))))

(setq org-tag-alist '((:startgroup)
                      ("personal" . ?p)
                      ("work" . ?w)
                      (:endgroup)))

(setq org-agenda-custom-commands
      '(("c" "Tasks by Status"
         ((todo "BACKLOG"
                ((org-agenda-overriding-header "Backlog")))
          (todo "WEEKLY"
                ((org-agenda-overriding-header "Weekly")))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting (on hold)")))
          (todo "TODO"
                ((org-agenda-overriding-header "TODO")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next TODO")))
          (todo "IN-PROGRESS"
                ((org-agenda-overriding-header "In-Progress")))
          (todo "DONE"
                ((org-agenda-overriding-header "Completed")))))))


(provide 'emacs-config-org)
