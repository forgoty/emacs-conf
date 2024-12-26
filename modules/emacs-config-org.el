(require 'org-super-agenda)

(setq org-directory "~/.local/share/org/")
(setq org-agenda-files (list org-directory))
(setq org-default-notes-file (concat org-directory "notes.org"))

(defun org-agenda-todo-next ()
    "Org agenda todo next cycle"
    (interactive)
    (org-call-with-arg 'org-agenda-todo 'right))

(defun org-agenda-todo-prev ()
    "Org agenda todo next cycle"
    (interactive)
    (org-call-with-arg 'org-agenda-todo 'left))

;; Define priority faces
(setq org-priority-faces
      '((?A . (:foreground "red" :weight bold))
        (?B . (:foreground "orange"))
        (?C . (:foreground "yellow"))))

;; Define todo keywords
(setq org-todo-keywords
      '((sequence "BACKLOG(b)" "WEEKLY(w)" "WAITING(h)" "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d)")))

;; custom faces for todo keywords
(setq org-todo-keyword-faces
      '(("BACKLOG" . (:foreground "gray" :weight bold))
        ("WEEKLY" . (:foreground "purple" :weight bold))
        ("TODO" . (:foreground "white" :weight bold))
        ("NEXT" . (:foreground "yellow" :weight bold))
        ("IN-PROGRESS" . (:foreground "orange" :weight bold))
        ("WAITING" . (:foreground "blue" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))))

;; Define tags
(setq org-tag-alist '((:startgroup)
                      ("personal" . ?p)
                      ("work" . ?w)
                      (:endgroup)))

;; Remove filename from the agenda view
(setq org-agenda-prefix-format "%t %s")

;; Custom agenda views
(org-super-agenda-mode)
(setq org-agenda-custom-commands
      '(("c" "Tasks"
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
                ((org-agenda-overriding-header "Completed")))))
        ("p" "Projects"
         ((todo ""
                   ((org-super-agenda-groups
                     '((:auto-parent t)))))))))

;; Capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+function org-default-notes-file org-goto)
         "* TODO %? %^g\n %u\n")))

(provide 'emacs-config-org)
