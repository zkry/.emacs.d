(require 'org)
(require 'org-capture)
(require 'org-agenda)
(require 'org-clock)
(require 'org-table)
(require 'org-habit)
(require 'org-duration)

(defun zr/org-write-schedule ()
  "Write a schedule."
  (interactive)
  (pcase-let* ((start-pos (point))
               (now (decode-time))
               (`(_ _ ,hour _ _ _ _ _ _) now))
    (while (< hour 23)
      (insert (format " - %d:00 : " hour))
      (insert "\n")
      (setq hour (1+ hour)))
    (org-indent-region start-pos (point))))

(global-set-key (kbd "C-c c") 'org-capture)

(define-key org-mode-map (kbd "C-c C-m") #'org-pomodoro)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@)" "|" "DONE(d)" "FROZEN(f)" "CANCELLED(c)")))

(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Dropbox/org/refile.org")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file "~/Dropbox/org/refile.org")
         "* %?\n  %i\n  %a")
        ("l" "Today I Learned" entry (file+headline "~/Dropbox/org/notesV2.org" "TIL")
         "** %<%d-%m-%Y> %?\n" :prepend t)
        ("j" "Journal Entry" entry (file+headline "~/Dropbox/org/notesV2.org" "Journal")
         "** %<%d-%m-%Y> %?\n" :prepend t)
        ("r" "+ Reading/Watching list" entry (file+headline "~/Dropbox/org/organizerV2.org" "Reading/Watching List")
         "** %?%^{Link}p%^{Topic|default|emacs|go|software|random|clojure|self-improvement}p\n")
        ("a" "Travel Audience Learn" entry (file+headline "~/Dropbox/org/ta.org" "Problems Solved Log")
         "** %<%d/%m> %?\n  %i\n  %a" :prepend t)))

(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
(setq org-global-properties (quote (("STYLE_ALL" . "habit pi"))))

(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w) ("@home" . ?h) ("@market" . ?m)
                      (:endgroup . nil)
                      ("deepw" . ?d)))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-agenda-files (list "~/Dropbox/org/organizerV2.org" "~/Dropbox/org/tickler.org"))
(setq org-default-notes-file "~/Dropbox/org/refile.org")

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "s-<up>") #'org-move-subtree-up)
            (local-set-key (kbd "s-<down>") #'org-move-subtree-down)
            (local-set-key (kbd "<M-S-return>") #'org-insert-todo-heading)
            (local-set-key (kbd "<C-S-return>") #'org-insert-todo-heading-respect-content)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (go . t)
   (java . t)
   (ruby . t)
   (latex . t)
   (scheme . t))) ; this line activates dot

(defun zr/sort-by-estimation ()
  (string-to-number (or (org-entry-get nil "ESTIMATION") "0")))

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(setq org-agenda-todo-ignore-deadlines nil)
(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))
(setq org-log-into-drawer t)

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    ;; also skip if future scheduled
    (let* ((scheduled-time (org-get-scheduled-time (point)))
           (future-scheduled (and scheduled-time (time-less-p (current-time) scheduled-time))))
      (when future-scheduled (setq should-skip-entry t)))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun zac/org-skip-future-scheduled ()
  (let* ((skip (save-excursion (org-entry-end-position)))
         (scheduled-time (org-get-scheduled-time (point)))
         (future-scheduled (and scheduled-time (time-less-p (current-time) scheduled-time))))
    (if future-scheduled skip nil)))


(setq org-agenda-custom-commands
      (quote (("g" "GTD"
               ((agenda "" nil)
                (tags-todo "gtd+assorted-@market"
                           ((org-agenda-overriding-header "Assorted Tasks")
                            (org-agenda-skip-function #'zac/org-skip-future-scheduled)))
                                        ;(tags-todo "gtd-assorted/NEXT"
                                        ;           ((org-agenda-overriding-header "Projects NEXT Tasks")
                                        ;            (org-agenda-todo-ignore-scheduled t)))
                (tags-todo "gtd-assorted-@market/TODO"
                           ((org-agenda-overriding-header "Project TODO Tasks")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
                (tags-todo "gtd+deepw-@market/TODO"
                           ((org-agenda-overriding-header "Deep Work Candidates")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-skip-function (lambda () (or (zac/org-skip-future-scheduled) (my-org-agenda-skip-all-siblings-but-first))))))))
              ("w" "Work"
               ((agenda "" nil)
                (tags-todo "+@office/TODO"
                           ((org-agenda-overriding-header "TODO Tasks")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first))))
               ((org-agenda-tag-filter-preset '("+@office")))))))

(setq org-agenda-todo-ignore-scheduled 'future)


;; TODO Find a better way to manage stuck projects. It seems like the 2nd level
;;      header shouldn't have a TODO tag.
(setq org-stuck-projects
      '("+LEVEL=2+proj/-DONE-CANCELLED"
        ("TODO" "NEXT")
        nil
        ""))


(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-src-fontify-natively t)
(setq org-latex-listing t)

(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(setq-default org-download-image-dir "./media")

(require 'org-roam)
(setq org-roam-directory "/Users/zromero/Dropbox/org/roam")
(add-hook 'after-init-hook 'org-roam-mode)
(define-key org-mode-map (kbd "C-c n i") #'org-roam-insert)
(define-key org-mode-map (kbd "C-c n I") #'org-roam-insert-immediate)
(define-key org-roam-mode-map (kbd "C-c n a t") #'org-roam-dailies-today)
(define-key org-roam-mode-map (kbd "C-c n a y") #'org-roam-dailies-yesterday)
(define-key org-roam-mode-map (kbd "C-c n a m") #'org-roam-dailies-tomorrow)
(define-key org-roam-mode-map (kbd "C-c n l") #'org-roam)
(define-key org-roam-mode-map (kbd "C-c n f") #'org-roam-find-file)
(define-key org-roam-mode-map (kbd "C-c n g") #'org-roam-graph)


(setq org-id-link-to-org-use-id t)

(require 'org-journal)
(define-key org-roam-mode-map (kbd "C-c n j") #'org-journal-new-entry)
(setq org-journal-date-prefix "#+title: "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-dir "/Users/zromero/Dropbox/org/roam/daily/"
      org-journal-date-format "%A, %d %B %Y"
      org-journal-time-prefix  "** ")

(require 'deft)
(define-key org-roam-mode-map (kbd "C-c n d") #'deft)
(setq deft-recursive t
      deft-use-filter-string-for-filename t
      deft-default-extension "org"
      deft-directory "/Users/zromero/Dropbox/org/roam")

(require 'org-fc)
(setq org-fc-directories '("/Users/zromero/Dropbox/org/"))
