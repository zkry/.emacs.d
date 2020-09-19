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

(message "Hello World!")

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
  (find-file "~/Dropbox/org/organizer.org"))

(defun zr/open-notes ()
  (interactive)
  (find-file "~/Dropbox/org/notesV2.org"))

(defun zr/til ()
  (interactive)
  (find-file "~/Dropbox/org/notesV2.org")
  (goto-char (point-min))
  (search-forward "* TIL")
  (insert (format "\n** %s " (format-time-string "%d-%m-%Y"))))

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

;; rainbow delimiter
(require 'rainbow-delimiters)
(setq rainbow-delimiters-mode nil)

;; smartparens
(smartparens-global-strict-mode t)

(setq super-save-mode nil)


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
        ("https://bzg.fr/index.xml" emacs)
        ("https://updates.orgmode.org/feed/help" org)
        ("https://defn.io/index.xml" racket)))


;; flycheck-clj-kondo
(require 'flycheck-clj-kondo)

;; yas-snippet
(yas-global-mode)

;; keybindings
(require 'smartparens)
(require 'mu4e)
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
(setq calendar-latitude 52.499
      calendar-longitude 13.436)
(setq awqat-asr-hanafi nil)
(awqat-set-preset-diyanet)
