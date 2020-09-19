(require 'org)
(require 'org-capture)
(require 'org-agenda)
(require 'org-clock)
(require 'org-table)

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "FROZEN(f)" "WAITING(w)" "CANCELLED(c)")))

(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Dropbox/org/refile.org")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file "~/Dropbox/org/refile.org")
         "* %?\n  %i\n  %a")))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-agenda-files (list "~/Dropbox/org/organizer.org" "~/Dropbox/org/tickler.org"))
(setq org-default-notes-file "~/Dropbox/org/refile.org")

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "s-<up>") #'org-move-subtree-up)
            (local-set-key (kbd "s-<down>") #'org-move-subtree-down)
            (local-set-key (kbd "<M-S-return>") #'org-insert-todo-heading)
            (local-set-key (kbd "<C-S-return>") #'org-insert-todo-heading-respect-content)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates dot

(defun zr/sort-by-estimation ()
  (string-to-number (or (org-entry-get nil "ESTIMATION") "0")))

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(setq org-agenda-todo-ignore-deadlines nil)
(setq org-agenda-todo-ignore-scheduled 'future)

(setq org-agenda-custom-commands
      (quote (("g" "GTD"
               ((agenda "" nil)
                (tags-todo "+proj-gtdtask-sleeping/NEXT"
                           ((org-agenda-overriding-header "Projects Next Tasks")
                            (org-agenda-todo-ignore-scheduled t)))
                (tags-todo "gtdtask/NEXT"
                           ((org-agenda-overriding-header "Assorted Tasks (NEXT)")
                            (org-agenda-todo-ignore-scheduled t)))
                (tags-todo "gtdtask/TODO"
                           ((org-agenda-overriding-header "Assorted Tasks (TODO)")
                            (org-agenda-todo-ignore-scheduled t)))))
              ("w" "Work"
               ((agenda "" nil)
                (tags-todo "+@office/NEXT"
                           ((org-agenda-overriding-header "TA NextTasks")
                            (org-agenda-todo-ignore-scheduled t)))
                (tags-todo "+@office/TODO"
                           ((org-agenda-overriding-header "ToDo Tasks")
                            (org-agenda-todo-ignore-scheduled t))))
               ((org-agenda-tag-filter-preset '("+@office")))))))
