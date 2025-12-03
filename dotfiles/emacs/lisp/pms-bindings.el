(define-prefix-command 'pms-kbd-map)
(global-set-key (kbd "s-a") 'pms-kbd-map)
(global-set-key (kbd "<C-f10>") 'pms-kbd-map)

(define-key pms-kbd-map (kbd "c l") 'pms-calc-eval-line)
(define-key pms-kbd-map (kbd "g s") 'magit-status)
(define-key pms-kbd-map (kbd "r f") 'pms-rg-files)
(define-key pms-kbd-map (kbd "r i") 'pms-random-ip)
(define-key pms-kbd-map (kbd "r m") 'pms-random-mac)
(define-key pms-kbd-map (kbd "r n") 'pms-random-number)
(define-key pms-kbd-map (kbd "r s") 'pms-random-string)
(define-key pms-kbd-map (kbd "s p") 'projectile-switch-project)
(define-key pms-kbd-map (kbd "t q") 'toggle-quotes)
(define-key pms-kbd-map (kbd "u b") 'pms-update-buffers)
(define-key pms-kbd-map (kbd "u u") 'pms-random-uuid)

(global-set-key (kbd "<C-f5>") 'pms-run-last-test)
(global-set-key (kbd "<f6>")   'next-error)
(global-set-key (kbd "<S-f6>") 'previous-error)

(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-f") 'projectile-find-file)

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "s-g") 'ido-goto-symbol)

(global-set-key (kbd "C-'") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "M-'") 'kmacro-end-or-call-macro)

(global-set-key (kbd "s-<left>") 'git-time-machine-diff-backwards)
(global-set-key (kbd "s-<right>") 'git-time-machine-diff-forwards)

(global-set-key (kbd "C-<tab>") 'company-manual-begin)

;; make M-` work like it does in macos
(global-set-key (kbd "M-`") 'other-frame)


;; remove suspend frame key binding
(global-unset-key (kbd "C-z"))

;; remove scroll-down-command binding
(global-unset-key (kbd "M-v"))

;; remove binding for kill-current-buffer
(global-unset-key (kbd "s-k"))

(provide 'pms-bindings)
