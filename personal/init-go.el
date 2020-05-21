(require 'smartparens)
(require 'projectile)

(add-hook 'go-mode-hook 'lsp)
;;(setq lsp-gopls-staticcheck t)

(defun zr/projectile-command (cmd &rest args)
  (let ((default-directory (projectile-project-root)))
    (unless default-directory (error "unable to find project root"))
    (condition-case nil
        (kill-buffer "*projectile-cmd*")
      (error nil))
    (with-current-buffer (get-buffer-create "*projectile-cmd*")
      (apply #'call-process (append (list cmd nil t nil) args))
      (special-mode)
      (switch-to-buffer (current-buffer)))))

(defun zr/projectile-go-vet ()
  (interactive)
  (zr/projectile-command "go" "vet" "./..."))

(defun zr/projectile-go-staticcheck ()
  (interactive)
  (zr/projectile-command "staticcheck" "./..."))

(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-t t") #'go-test-current-test)
            (local-set-key (kbd "C-c C-t b") #'go-test-current-benchmark)
            (local-set-key (kbd "C-c C-t f") #'go-test-current-file)
            (local-set-key (kbd "C-c C-t C-f b") #'go-test-current-file-benchmarks)
            (local-set-key (kbd "C-c C-t a") #'go-test-current-project)
            (local-set-key (kbd "C-c C-t c") #'go-test-current-coverage)
            (local-set-key (kbd "C-c C-t C-a b") #'go-test-current-project-benchmarks)

            ;; smartparens
            (local-set-key (kbd "C-S-(") #'sp-slurp-hybrid-sexp)
            (local-set-key (kbd "C-M-t") #'sp-transpose-hybrid-sexp)
            (local-set-key (kbd "C-M-T") #'sp-push-hybrid-sexp)
            (local-set-key (kbd "<C-tab>") #'sp-indent-adjust-sexp)
            (local-set-key (kbd "<C-M-tab>") #'sp-dedent-adjust-sexp)

            (local-set-key (kbd "C-c C-p v") #'zr/projectile-go-vet)
            (local-set-key (kbd "C-c C-p s") #'zr/projectile-go-staticcheck)))
