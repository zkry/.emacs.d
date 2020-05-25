(require 'org)
(require 'org-capture)

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
            (local-set-key (kbd "<M-S-return>") #'org-insert-todo-heading-respect-content)))

(defun zr/sort-by-estimation ()
  (string-to-number (or (org-entry-get nil "ESTIMATION") "0")))
