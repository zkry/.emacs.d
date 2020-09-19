(provide 'init-go)

;;; init-go.el ends here
(require 'smartparens)
(require 'projectile)
(require 'lsp-go)
(require 'go-mode)
;;(require 'ttest)

(add-hook 'go-mode-hook 'lsp-deferred)
;;(setq lsp-gopls-staticcheck t)
(setq gofmt-command "goimports")

(defconst zr/gopath "/Users/zromero/go/")
(defconst zr/mygodir (concat zr/gopath "src/github.com/zkry/"))

(defun zr/go-new-project (dir-name)
  (interactive "sDirName:")
  (let* ((dir-name (concat zr/mygodir dir-name))
         (dir-ok (call-process "mkdir" nil nil nil dir-name)))
    (when (> dir-ok 1) (error (concat "unable to create directory " dir-name)))
    (let* ((default-directory dir-name)
           (gomod-init-ok (call-process "go" nil nil nil "mod" "init")))
      (when (> gomod-init-ok 1) (error (concat "unable to init go mod in dir " dir-name)))
      (find-file (concat dir-name "/go.mod")))))


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

(defun zr/go-remove-error ()
  "Remove the error for the current function."
  (save-excursion
    (go-goto-return-values)
    (let ((multi-args (save-excursion (backward-char) (looking-at "("))))
      (if multi-args
          (progn
            (while (not (looking-at "error")) (forward-char))
            (while (and (not (looking-at ","))
                        (not (looking-at "(")))
              (backward-char))
            (let ((beg (point))
                  (whole-parens (looking-at "(")))
              (search-forward ")")
              (backward-char)
              (delete-region beg (point))
              (when whole-parens (delete-char 1))))
        (let ((beg (point)))
          (search-forward "{")
          (backward-char)
          (delete-region beg (point)))))))

(defun zr/go-add-error ()
  "Add an error return to the current function."
  (save-excursion
    (go-goto-return-values)
    (if (looking-at " {")
        (insert "error")
      (let ((multi-args (save-excursion (backward-char) (looking-at "("))))
        (while (not (looking-at "[a-zA-Z_]")) (forward-char))
        (if multi-args
            (let ((named-args (looking-at "[a-zA-Z0-9_]+ [a-zA-Z0-9_]+")))
              (while (not (looking-at ")")) (forward-char))
              (if (save-excursion (backward-char) (looking-at "\n"))
                  (if named-args
                      (insert "\terr error,\n")
                    (insert "\terror,\n"))
                (if named-args
                    (insert ", err error")
                  (insert ", error"))))
          (insert "(")
          (while (not (looking-at " ")) (forward-char))
          (insert ", error)"))))))

(defun zr/go-toggle-error ()
  "Toggle the return error value on a function."
  (interactive)
  (save-excursion
    (go-goto-return-values)
    (let ((has-error nil))
      (while (and (not has-error) (not (looking-at "{")))
        (when (looking-at "error")
          (setq has-error t))
        (forward-char))
      (if has-error
          (zr/go-remove-error)
        (zr/go-add-error)))))

(add-hook 'go-mode-hook
          (lambda ()
            ;; C-c C-a go-import-add
            (local-set-key (kbd "C-c C-b f") #'go-ttest-add-field)
            (local-set-key (kbd "C-c C-b t") #'go-ttest)
            (local-set-key (kbd "C-c C-b a") #'go-ttest-add-test)
            ;; C-c C-c
            ;; C-c C-d godef-describe
            ;; C-c C-e
            (local-set-key (kbd "C-c C-e") #'zr/go-toggle-error)
            ;; C-c C-f go-goto-...
            ;; C-c C-g
            ;; C-c C-h
            ;; C-c C-i
            ;; C-c C-j
            ;; C-c C-j godef-jump-...
            ;; C-c C-l
            ;; C-c C-m
            ;; C-c C-n
            ;; C-c C-o go-guru-...
            (local-set-key (kbd "C-c C-p v") #'zr/projectile-go-vet)
            (local-set-key (kbd "C-c C-p s") #'zr/projectile-go-staticcheck)
            ;; C-c C-q
            ;; C-c C-r
            ;; C-c C-s
            (local-set-key (kbd "C-c C-t t") #'go-test-current-test)
            (local-set-key (kbd "C-c C-t b") #'go-test-current-benchmark)
            (local-set-key (kbd "C-c C-t f") #'go-test-current-file)
            (local-set-key (kbd "C-c C-t C-f b") #'go-test-current-file-benchmarks)
            (local-set-key (kbd "C-c C-t a") #'go-test-current-project)
            (local-set-key (kbd "C-c C-t c") #'go-test-current-coverage)
            (local-set-key (kbd "C-c C-t C-a b") #'go-test-current-project-benchmarks)
            ;; C-c C-u
            ;; C-c C-v
            ;; C-c C-w
            ;; C-c C-x
            ;; C-c C-y
            ;; C-c C-z






            ;; smartparens
            (local-set-key (kbd "C-S-(") #'sp-slurp-hybrid-sexp)
            (local-set-key (kbd "C-M-t") #'sp-transpose-hybrid-sexp)
            (local-set-key (kbd "C-M-T") #'sp-push-hybrid-sexp)
            (local-set-key (kbd "<C-tab>") #'sp-indent-adjust-sexp)
            (local-set-key (kbd "<C-M-tab>") #'sp-dedent-adjust-sexp)

            (local-set-key (kbd "C-c C-p v") #'zr/projectile-go-vet)
            (local-set-key (kbd "C-c C-p s") #'zr/projectile-go-staticcheck)))

(provide 'init-go)
;;; init-go.el ends here
