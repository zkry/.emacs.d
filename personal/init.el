;; configuration of path
(setenv "PATH"
        (concat
         (getenv "PATH")
         ":/Users/zromero/go/bin/"
         ":/usr/local/go/bin/"
         ":/Users/zromero/bin"))

(setenv "GOPRIVATE" "github.com/travelaudience/")
(setenv "GOPROXY" "direct")
(setenv "PURE_GIT_PULL" "0")
(setenv "GOSUMDB" "off")

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
  (find-file "~/Dropbox/org/notes.org"))

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
  (define-key map (kbd "L h") 'git-link-homepage)
  (define-key map (kbd "L l") 'git-link)
  (define-key map (kbd "L c") #'git-link-commit))

;; avy
(require 'avy)
(setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))

;; ace
(require 'ace-window)
(setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o))

;; magit-circleci
(require 'magit-circleci)
(setq magit-circleci-token "79f490d972d5df482eacfc0c6dac7daa311d19aa")
(magit-circleci-mode)

;; rainbow delimiter
(require 'rainbow-delimiters)
(setq rainbow-delimiters-mode nil)

;; smartparens
(smartparens-global-strict-mode t)

(setq super-save-mode nil)

;; yas-snippet
(yas-global-mode)

;; keybindings
(global-set-key (kbd "C-§ i") #'zr/open-init)
(global-set-key (kbd "C-§ o") #'zr/open-organizer)
(global-set-key (kbd "C-§ n") #'zr/open-notes)
(global-set-key (kbd "C-§ r") #'zr/open-refile)
(global-set-key (kbd "<C-M-backspace>") #'sp-splice-sexp-killing-backward)
(global-set-key (kbd "C-M-]") #'sp-rewrap-sexp)
