(require 'go-mode "/home/psanford/projects/go-mode.el/go-mode.el")

(defvar pms-go-mode-map (make-sparse-keymap)
  "Bindings for pms-go-mode with prefix maps")

(define-prefix-command 'pms-go-prefix-map)
(define-key pms-go-prefix-map (kbd "p t") 'pms-go-single-test-function)

(define-key pms-go-mode-map (kbd "<f4>") 'pms-go-run-test)
(define-key pms-go-mode-map (kbd "<f5>") 'pms-go-run-test)
(define-key pms-go-mode-map (kbd "<C-s-f4>") 'pms-go-run-buffer)
(define-key pms-go-mode-map (kbd "<C-s-f5>") 'pms-go-run-buffer)
(define-key pms-go-mode-map (kbd "<S-f4>") 'pms-go-find-other-file)
(define-key pms-go-mode-map (kbd "<S-f5>") 'pms-go-find-other-file)
(define-key pms-go-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
(define-key pms-go-mode-map (kbd "<s-f4>") 'pms-go-single-test-function)
(define-key pms-go-mode-map (kbd "<s-f5>") 'pms-go-single-test-function)
(define-key pms-go-mode-map (kbd "s-n") 'pms-go-prefix-map)
(define-key pms-go-mode-map (kbd "s-a") 'pms-go-prefix-map)

;; mirror xref bindings
;; (define-key pms-go-mode-map (kbd "M-.") 'godef-jump)
;; (define-key pms-go-mode-map (kbd "M-,") 'pop-tag-mark)

(setq gofmt-command "goimports")
(setq godoc-command "go doc -all")
(setq go-packages-function 'go-packages-go-list)

(add-to-list 'compilation-error-regexp-alist-alist
             '(go-compilation
               "\\([^[:space:]\n:]+\\.go\\):\\([0-9]+\\)" 1 2))

(define-compilation-mode go-compilation "Go Compile"
  "Compilation mode for go"
  (set (make-local-variable 'compilation-error-regexp-alist) '(go-compilation)))

(defun pms-go-signal-compilation-sigquit ()
  "Send SIGQUIT to process assocated with buffer"
  (interactive)
  (let ((buffer (compilation-find-buffer)))
    (if (get-buffer-process buffer)
        (quit-process (get-buffer-process buffer))
      (error "The %s process is not running" (downcase mode-name)))))

(defun pms-go-run-buffer ()
  "Runs tests if not package main, otherwise runs file"
  (interactive)

  (if (and (not (string-match "\\(.*\\)_test\.go$" (buffer-file-name)))
           (save-excursion
             (save-match-data
               (goto-char (point-min))
               (search-forward "package main" nil t))))
      (pms-go-run)
    (pms-go-run-test)))

(defun pms-go-run-test (&optional more-test-args)
  "Runs tests for current package"
  (interactive)

  (when (buffer-file-name)
    (save-buffer))

  (compile (concat "go test -v -count=1 -mod=mod" more-test-args) 'go-compilation)

  (ring-insert pms-previous-test-ring
               (cons
                `(lambda () (pms-go-run-test ,more-test-args))
                (current-buffer))))

(defun pms-go-single-test-function (&optional more-test-args)
  "Runs the current test function only"
  (interactive)

  (save-excursion
    (let ((func-name (go--function-name)))
      (when (not (string-prefix-p "Test" func-name))
        (error "Not in a test function: %s" func-name))

      (compile (concat "go test -v -count=1 -mod=mod -run ^" func-name "$" more-test-args) 'go-compilation)

      (ring-insert pms-previous-test-ring
                   (cons
                    `(lambda () (pms-go-single-test-function ,more-test-args))
                    (current-buffer))))))

(defun pms-go-run ()
  "Run current file"
  (interactive)

  (when (buffer-file-name)
    (save-buffer))

  (compile (concat "go run " (buffer-file-name)) 'go-compilation)

  (ring-insert pms-previous-test-ring
               (cons
                `(lambda () (pms-go-run))
                (current-buffer))))

(defun pms-gojson-region (beg end)
  "Run gojson on a region"
  (interactive "r")
  (shell-command-on-region beg end "gojson" nil t nil t))

(defun pms-go-mod-init ()
  "Run go mod init for current directory"
  (interactive)
  (let ((pkgname (file-name-base (directory-file-name default-directory))))
    (call-process "go" nil nil nil "mod" "init" (concat "github.com/psanford/" pkgname))))

(defun pms-go-run ()
  "Run current file"
  (interactive)
  (compile (concat "go run -mod=mod " (buffer-file-name)) 'go-compilation))

(defun pms-set-go-path ()
  "Search up for a go root directory and set GOPATH to that"
  (interactive)
  (let ((f (buffer-file-name)))
    (if (string-match "^\\(.*/go\\)/src.*$" f)
        (progn
          (message "GOPATH=%s" (match-string 1 f))
          (setenv "GOPATH" (match-string 1 f)))
      (message "No go directory found"))))

(defun pms-go-find-other-file ()
  "Toggle between test and code"
  (interactive)
  (let ((current-filename (buffer-file-name))
        (other-file))
    (setq other-file
          (cond
           ((string-match "\\(.*\\)_test\.go$" current-filename)
            (concat (match-string 1 current-filename) ".go"))
           ((string-match "\\(.*\\)\.go$" current-filename)
            (concat (match-string 1 current-filename) "_test.go"))
           (t (error "no file found"))))
    (if (file-exists-p other-file)
        (find-file other-file)
      (when (yes-or-no-p (format "File not found %s, create?" other-file))
        (find-file other-file)))))

(add-hook 'go-mode-hook #'lsp-deferred)

(defun pms-gofmt-r ()
  "Rewrite the current buffer by applying the result of 'gofmt -r' to it"
  (interactive)
  (let ((rule (read-string "Enter gofmt -r rewrite rule: ")))
    (if (string-empty-p rule)
        (message "No rewrite rule provided. Aborting.")
      (let ((buffer-contents (buffer-string)))
        (with-temp-buffer
          (insert buffer-contents)
          (if (zerop (call-process-region (point-min) (point-max) "gofmt" t t nil "-r" rule))
              (let ((rewritten-contents (buffer-string)))
                (with-current-buffer (current-buffer)
                  (erase-buffer)
                  (insert rewritten-contents)
                  (message "Buffer rewritten with gofmt -r rule: %s" rule)))
            (message "Error applying gofmt -r rule: %s" rule)))))))


(defun pms-go-fix-ioutil ()
  "Replace deprecated ioutil functions with their non-deprecated alternatives using gofmt."
  (interactive)
  (let ((replacements
         '(("ioutil.NopCloser(a)" . "io.NopCloser(a)")
           ("ioutil.ReadAll(a)" . "io.ReadAll(a)")
           ("ioutil.ReadDir(a)" . "os.ReadDir(a)")
           ("ioutil.ReadFile(a)" . "os.ReadFile(a)")
           ("ioutil.TempDir(a, b)" . "os.MkdirTemp(a, b)")
           ("ioutil.TempFile(a, b)" . "os.CreateTemp(a, b)")
           ("ioutil.WriteFile(a, b, c)" . "os.WriteFile(a, b, c)"))))
    (dolist (replacement replacements)
      (let ((from (car replacement))
            (to (cdr replacement)))
        (shell-command
         (format "gofmt -w -r '%s -> %s' %s"
                 from to (buffer-file-name)))
        (message "Replaced %s with %s" from to)))
    (revert-buffer t t)
    (when lsp-mode
      (lsp-organize-imports))
    (message "Finished replacing deprecated ioutil functions.")))

;; https://github.com/golang/tools/blob/master/gopls/doc/settings.md
(lsp-register-custom-settings
 '(
   ("gopls.completeUnimported" t t)
   ("gopls.experimentalPostfixCompletions" t t)
   ;; ("gopls.allExperiments" t t)
   ;; ("gopls.deepCompletion" t t)
   ;;("gopls.staticcheck" t t)

   ;; gopls.buildFlags []string
   ;; gopls.env map
   ;; gopls.hoverKind string
   ;; gopls.experimentalDisabledAnalyses []string
   ;; completionDocumentation bool
   ))

(define-minor-mode pms-go-mode
  "Minor mode for go-specific functions

Commands:
\\{pms-go-mode-map}
"
  :init-value nil
  :keymap pms-go-mode-map

  ;; set APPEND flag so they run in order defined (so vet hook
  ;; can avoid running if gofmt failed)
  ;; (add-hook 'before-save-hook 'gofmt-before-save t t)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  )

(add-hook 'go-mode-hook
          (lambda ()
            (pms-go-mode t)))

(provide 'pms-go)
