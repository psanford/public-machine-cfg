(defvar pms-rust-mode-map (make-sparse-keymap)
  "Bindings for pms-rust-mode with prefix maps")

(define-prefix-command 'pms-rust-prefix-map)
(define-key pms-rust-prefix-map (kbd "p t") 'pms-rust-single-test-function)

(define-key pms-rust-mode-map (kbd "<f4>") 'pms-rust-run-test)
(define-key pms-rust-mode-map (kbd "<f5>") 'pms-rust-run-test)
(define-key pms-rust-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
(define-key pms-rust-mode-map (kbd "s-n") 'pms-rust-prefix-map)
(define-key pms-rust-mode-map (kbd "s-a") 'pms-rust-prefix-map)


(defun pms-rust-run-test (&optional more-test-args)
  "Runs tests for current package"
  (interactive)

  (when (buffer-file-name)
    (save-buffer))

  (compile (concat "cargo test" more-test-args))

  (ring-insert pms-previous-test-ring
               (cons
                `(lambda () (pms-rust-run-test ,more-test-args))
                (current-buffer))))


(add-hook 'rust-mode-hook #'lsp-deferred)

(define-minor-mode pms-rust-mode
  "Minor mode for go-specific functions

Commands:
\\{pms-rust-mode-map}
"
  :init-value nil
  :keymap pms-rust-mode-map

  ;; set APPEND flag so they run in order defined (so vet hook
  ;; can avoid running if gofmt failed)
  ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'before-save-hook #'rust-format-buffer t t)
  )

(add-hook 'rust-mode-hook
          (lambda ()
            (pms-rust-mode t)))


(provide 'pms-rust)
