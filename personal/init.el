;;; init.el --- My Emacs initialization file -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ()

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Code:

(provide 'init)

;;; init.el ends here
;; configuration of path
(setenv "PATH"
        (concat
         (getenv "PATH")
         ":/Users/zromero/go/bin"
         ":/usr/local/go/bin"
         ":/Users/zromero/bin"
         ":/Users/zromero/.local/bin"))

(setenv "GOPRIVATE" "github.com/travelaudience/")
(setenv "GOPROXY" "direct")
(setenv "PURE_GIT_PULL" "0")
(setenv "GOSUMDB" "off")
(setenv "GOPATH" "/Users/zromero/go")
(setenv "GOBIN" "/Users/zromero/go/bin")

(toggle-scroll-bar -1)

;; related to lsp performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(defun zr/open-init ()
  (interactive)
  (find-file "~/.emacs.d/personal/init.el"))

(defun zr/open-refile ()
  (interactive)
  (find-file "~/Dropbox/org/refile.org"))

(defun zr/open-organizer ()
  (interactive)
  (find-file "~/Dropbox/org/organizerV2.org"))

(defun zr/open-notes ()
  (interactive)
  (find-file "~/Dropbox/org/notesV2.org"))

(defun zr/til ()
  (interactive)
  (find-file "~/Dropbox/org/notesV2.org")
  (goto-char (point-min))
  (search-forward "* TIL")
  (insert (format "\n** %s " (format-time-string "%d-%m-%Y"))))

(require 'org-pomodoro)
(defconst zr/org-pomodoro-bitbar-file-name
  "~/dev/bitbar/org-pomodoro.10s.sh"
  "The name of the org-pomodoro file for bitbar to see.")
(defvar zr/org-pomodoro-ticker
  0
  "Ticker to keep track of how many ticks called.")

(defun zr/org-pomodoro-color (time)
  "Calculate the color to display TIME in bitbar."
  (if (equal org-pomodoro-state ':short-break)
      "blue"
    (let* ((parts (split-string time ":"))
           (mins (string-to-number (car parts)))
           (secs (string-to-number (cadr parts))))
      (if (< mins 5) "red" "black"))))

(defun zr/org-pomodoro-update-bitbar ()
  "Update bitbar with pomodoro time."
  (let* ((time-str (org-pomodoro-format-seconds))
         (color-str (zr/org-pomodoro-color time-str ))
         (file-name (format "~/pomodoro.txt")))
    (if (equal "00:00" time-str)
        (shell-command-to-string (format "echo '0' > %s" file-name))
      (shell-command-to-string (format "echo '🍅%s | color=\"%s\"' > %s" (org-pomodoro-format-seconds) color-str file-name)))))



(defvar zr/org-clock--timer nil "Timer to write to file.")

(defun zr/org-clock-update-bitbar ()
  (let* ((time-secs (float-time (time-subtract (current-time) org-clock-start-time)))
         (mins (/ time-secs 60))
         (secs (mod time-secs 60))
         (file-name "~/pomodoro.txt")
         (time-str (format "%02dm%02ds" mins secs)))
    (shell-command-to-string (format "echo '%s' > %s" time-str file-name))))

(defun zr/org-clock-start-timer ()
  (message "starting org-clock timer")
  (when (> (float-time (time-subtract (current-time) org-pomodoro-last-clock-in)) 1)
    ;; you can start org timer printer
    (when zr/org-clock--timer
      (cancel-timer zr/org-clock--timer))
    (setq zr/org-clock--timer (run-with-timer 5 5 #'zr/org-clock-update-bitbar))))
(defun zr/org-clock-stop-timer ()
  (message "stopping org-clock timer")
  (when zr/org-clock--timer
    (zr/org-pomodoro-delete-bitbar-file)
    (cancel-timer zr/org-clock--timer)
    (setq zr/org-clock--timer nil)))


(defun zr/org-pomodoro-bitbar-tick ()
  (setq zr/org-pomodoro-ticker (1+ zr/org-pomodoro-ticker))
  (when (= (mod zr/org-pomodoro-ticker 10) 0)
    (zr/org-pomodoro-update-bitbar)))

(defun zr/org-pomodoro-start-bitbar-file ()
  "Create bitbar file."
  ;; (shell-command-to-string
  ;;  (concat (format "echo '#!/bin/bash' > %s" zr/org-pomodoro-bitbar-file-name)
  ;;          ";"
  ;;          (format "echo 'cat /Users/zromero/pomodoro.txt' >> %s" zr/org-pomodoro-bitbar-file-name)
  ;;          ";"
  ;;          (format "chmod +x %s" zr/org-pomodoro-bitbar-file-name)))
  (zr/org-pomodoro-update-bitbar))

(defun zr/org-pomodoro-delete-bitbar-file ()
  "Delete the org Pomodoro bitbar file."
  (shell-command-to-string
   "echo '0' > ~/pomodoro.txt"))

(add-hook 'org-pomodoro-tick-hook 'zr/org-pomodoro-bitbar-tick)
(add-hook 'org-pomodoro-started-hook 'zr/org-pomodoro-start-bitbar-file)
(add-hook 'org-pomodoro-killed-hook 'zr/org-pomodoro-delete-bitbar-file)
(add-hook 'org-pomodoro-finished-hook 'zr/org-pomodoro-delete-bitbar-file)
(add-hook 'org-pomodoro-break-finished-hook 'zr/org-pomodoro-delete-bitbar-file)

(setq org-pomodoro-finished-sound "/Users/zromero/dev/emacs/alarm.wav")
(setq org-pomodoro-short-break-sound "/Users/zromero/dev/emacs/positive-blip.wav")
(setq org-pomodoro-long-break-sound "/Users/zromero/dev/emacs/positive-blip.wav")

(add-hook 'org-clock-in-hook 'zr/org-clock-start-timer)
(add-hook 'org-clock-out-hook 'zr/org-clock-stop-timer)

(require 'whitespace)
(setq-default tab-width 4)
(setq-default whitespace-style '(face tabs empty trailing))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; git-link
(require 'git-link)
(require 'projectile)
(setq git-link-open-in-browser t)
(let ((map projectile-command-map))
  (define-key map (kbd "n h") 'git-link-homepage)
  (define-key map (kbd "n l") 'git-link)
  (define-key map (kbd "n c") #'git-link-commit))

;; avy
(require 'avy)
(setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))

;; ace
(require 'ace-window)
(setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o))

(require 'turkish)

;; magit-circleci
                                        ;(require 'magit-circleci)
                                        ;(setq magit-circleci-token "79f490d972d5df482eacfc0c6dac7daa311d19aa")
                                        ;(magit-circleci-mode)

;; diminish
(diminish 'guru-mode)
(diminish 'yas-minor-mode)
(diminish 'flycheck-mode)
(diminish 'company-mode)
(diminish 'ivy-mode)
(diminish 'editorconfig-mode)
(diminish 'whitespace-mode)
(defun zr/shorten-name (name)
  (message name)
  (cond
    ((= name "tde-delivery-engine") "DE")
    (t "???")))
(diminish 'projectile-mode '(:eval (format " Prj(%s)" (projectile-project-name))))
(diminish 'flyspell-mode)
(diminish 'prelude-mode)
(diminish 'which-key-mode)
(diminish 'beacon-mode)
(diminish 'org-table-header-line-mode)
(diminish 'abbrev-mode)
(diminish 'subword-mode)
(diminish 'smartparens-mode)
(diminish 'intentional-minor-mode)
(diminish 'org-roam-mode)
(diminish 'guru-mode)

;; rainbow delimiter
(require 'rainbow-delimiters)
(setq rainbow-delimiters-mode nil)

;; smartparens
(smartparens-global-strict-mode t)

(setq super-save-mode nil)

;; ibuffer
(require 'ibuffer-projectile)
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; elfeed
(setq elfeed-feeds
      '("https://metaredux.com/feed.xml"
        ("https://blog.cleancoder.com/atom.xml" dev)
        ("https://css-tricks.com/feed/" frontend)
        ("http://nullprogram.com/feed/" blog emacs)
        ("https://www.reddit.com/r/Clojure/.rss" clojure)
        ("https://golangnews.com/index.xml" golang)
        ("https://changelog.com/gotime/feed" golang)
        ("https://www.ardanlabs.com/blog/index.xml" golang)
        ("https://www.cncf.io/feed" k8s)
        ("https://kubernetes.io/feed.xml" k8s)
        ;; ("https://www.reddit.com/r/emacs/.rss" emacs)
        ("https://sachachua.com/blog/feed/" emacs)
        ("http://ergoemacs.org/emacs/blog.xml" emacs)
        ("https://emacsredux.com/feed.xml" emacs)
        ("https://irreal.org/blog/?feed=rss2" emacs)
        ("http://blog.binchen.org/rss.xml" emacs)
        ("https://200ok.ch/atom.xml" emacs)
        ("https://bzg.fr/index.xml" emacs)
        ("https://updates.orgmode.org/feed/help" org)
        ("https://defn.io/index.xml" racket)))


;; flycheck-clj-kondo
(require 'flycheck-clj-kondo)

;; yas-snippet
(yas-global-mode)

;; keybindings
(require 'smartparens)

(global-set-key (kbd "C-§ i") #'zr/open-init)
(global-set-key (kbd "C-§ o") #'zr/open-organizer)
(global-set-key (kbd "C-§ n") #'zr/open-notes)
(global-set-key (kbd "C-§ r") #'zr/open-refile)
(global-set-key (kbd "<C-M-backspace>") #'sp-splice-sexp-killing-backward)
(global-set-key (kbd "C-M-]") #'sp-rewrap-sexp)
(global-set-key (kbd "C-M-SPC") #'sp-mark-sexp)

;; C-c a org-agenda
;; C-c b org-switchb
;; C-c c org-capture
;; C-c d crux-duplicate-current-line-or-region
;; C-c e crux-eval-and-replace
;; C-c f crux-recentf-find-file
;; C-c g prelude-google
;; C-c h
;; C-c i imenu-anywhere
;; C-c j counsel-git-grep
;; C-c k crux-kill-other-buffers
;; C-c l org-store-link
(global-set-key (kbd "C-c m") #'mu4e)
;; C-c n crux-cleanup-buffer-or-region
;; C-c o crux-open-with
;; C-c p ... projectile-...
(global-set-key (kbd "C-c q") #'awqat-times-for-day)
;; C-c r crux-rename-buffer-and-file
;; C-c s crux-swap-windows
;; C-c t crux-visit-term
;; C-c u crux-view-url
;; C-c v
(global-set-key (kbd "C-x w") #'elfeed)
;; C-c w prelude-swap-meta-and-super
;; C-c x
;; C-c y prelude-youtube
;; C-c z
;; C-c D crux-delete-file-and-buffer
;; C-c G prelude-github
;; C-c I crux-find-user-init-file
;; C-c S crux-find-shell-init-file
;; C-c U prelude-duckduckgo
;; C-c & ... yas-...
;; C-c ! ... flycheck-
;; C-c <left> winner-undo
;; C-c <right> winner-redo
;; C-c $ flyspell-correct-word-before-point
;; C-c TAB crux-indent-rigidly-and-copy-to-clipboard
;; C-c . ... apply-operation-to-number-at-point

(setq auth-sources '("~/.authinfo"))
(require 'forge)


(require 'awqat)
;; Berlin
(setq calendar-latitude 52.499
      calendar-longitude 13.436)
;; AZ
;;(setq calendar-latitude 40.98
;;      calendar-longitude 28.8)

(setq awqat-asr-hanafi nil)
(awqat-set-preset-diyanet)
(setq awqat-fajr-angle -20.70)
(setq awqat-isha-angle -16.3)
(setq awqat-prayer-safety-offsets
      '(0.0 0.0 7.0 -9.0 0.0 0.0))

(setq term-prompt-regexp "[^\n%]*% *")

;; geiser
(require 'geiser)
(put 'fresh 'scheme-indent-function 1)


(require 'quail)

;; Intentional
(require 'intentional)

(setq intentional-output-file "~/browse-intentions.json")

(setq intentional-global-intentions
      '(("Dev Work" always ("http://localhost:3000/*"))
        ("Language Goals" always ("translate.google.com/*"))
        ("Work on-call" always ("https://*.pagerduty.com/*"))
        ("Work" (between-on-days "08:00" "18:00" (1 2 3 4 5))
         ("https://*.atlassian.net/*"
          "https://github.com/*"
          "https://outlook.office.com/*"
          "https://golang.org/*"
          "https://*circleci.com/*"
          "https://accounts.google.com/*"
          "googleusercontent.com/*"
          "travelaudience.com/*"
          "codeclimate.com/*"))
        ("Relax" (between "19:00" "21:00") ("https://youtube.com/*"))))

(setq intentional-site-groups
      '(("work" "https://*.atlassian.net/*" "https://github.com/*" "https://outlook.office.com/*" "https://golang.org/*" "travelaudience.com/*" "circleci.com/*")
        ("clojure" "https://clojuredocs.org/*" "https://clojure.org/*" "https://cljdoc.org/*" "https://github.com/*" "https://clojureverse.org/*" "https://stackoverflow.com/*")
        ("golang" "https://golang.org/*" "https://github.com/*" "https://godoc.org/*" "https://stackoverflow.com/*")))

(setq intentional-tag-intentions
      '(("shopping" ("amazon.com/*" "amazon.de/*"))
        ("deepw" ("mynoise.net/*"))
        ("clojure" ("clojure"))))

(setq intentional-save-to-journal nil)

(setq intentional-extract-clock-body-urls t)


;;; Java LSP
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
