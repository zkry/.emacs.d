(when (not (string-equal system-type "darwin"))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq prelude-theme 'leuven)

(add-to-list 'load-path "~/dev/emacs/org-mode/lisp/")
(require 'org-loaddefs)
(require 'org-duration)

(add-to-list 'load-path "~/dev/emacs/org-mode/contrib/lisp/")

(add-to-list 'load-path "~/dev/emacs/mu/mu4e")

(add-to-list 'load-path "~/dev/emacs/awqat")

(add-to-list 'load-path "~/dev/emacs/time-table")
(require 'time-table)

(add-to-list 'load-path "~/dev/emacs/org-roam")

(add-to-list 'load-path "~/dev/emacs/org-fc/")
(require 'org-fc)
;(require 'org-fc-hydra)


(add-to-list 'load-path "~/dev/emacs/browse-by-intention/")
(require 'intentional)
