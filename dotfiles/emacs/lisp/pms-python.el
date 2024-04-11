(defvar pms-python-mode-map (make-sparse-keymap)
  "Bindings for pms-python-mode with prefix maps")

(define-prefix-command 'pms-python-prefix-map)
(define-key pms-python-mode-map (kbd "<f4>") 'pms-python-run-buffer)
(define-key pms-python-mode-map (kbd "<f5>") 'pms-python-run-buffer)
(define-key pms-python-mode-map (kbd "<C-s-f4>") 'pms-python-run-buffer)
(define-key pms-python-mode-map (kbd "<C-s-f5>") 'pms-python-run-buffer)
;; (define-key pms-python-mode-map (kbd "<S-f4>") 'pms-python-find-other-file)
;; (define-key pms-python-mode-map (kbd "<S-f5>") 'pms-python-find-other-file)
(define-key pms-python-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
;; (define-key pms-python-mode-map (kbd "<s-f4>") 'pms-python-single-test-function)
;; (define-key pms-python-mode-map (kbd "<s-f5>") 'pms-python-single-test-function)
(define-key pms-python-mode-map (kbd "s-n") 'pms-python-prefix-map)
(define-key pms-python-mode-map (kbd "s-a") 'pms-python-prefix-map)


(defun pms-python-run-buffer ()
  "Excute current file"
  (interactive)

  (when (buffer-file-name)
    (save-buffer))

  (compile (concat "python " (buffer-file-name)))
  (ring-insert pms-previous-test-ring
               (cons
                `(lambda () (pms-python-run-buffer))
                (current-buffer))))


(define-minor-mode pms-python-mode
  "Minor mode for python-specific functions

Commands:
\\{pms-python-mode-map}
"
  :init-value nil
  :keymap pms-python-mode-map
  )

(add-hook 'python-mode-hook
          (lambda ()
            (pms-python-mode t)))

(provide 'pms-python)
