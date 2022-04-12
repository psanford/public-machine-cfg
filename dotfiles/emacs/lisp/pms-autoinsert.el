; new file templates
(require 'autoinsert)
(auto-insert-mode t)
(setq auto-insert-directory (concat dotfiles-dir "/templates/"))
(setq auto-insert-query nil)
(setq auto-insert-alist
      '(
        ("^/tmp/main\\.go$" . ["go_main_template"])
        ("_test\\.go$" . ["go_test_template" pms-templatize-go-test])
        ("\\.go$" . ["go_template" pms-templatize-go])
        ("LICENSE" . ["mit_license_template" pms-templatize-mit-license])
        ))
(setq auto-insert 'other)

(defun pms-templatize-go ()
  (let ((package (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name))))))
    (beginning-of-buffer)
    (replace-string "!!!PACKAGE!!!" package)))

(defun pms-templatize-go-test ()
  (let ((package (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name))))))
    (beginning-of-buffer)
    (replace-string "!!!PACKAGE!!!" package)
    (beginning-of-buffer)
    (replace-string "!!!UPCASE_PACKAGE!!!" (capitalize package))))

(defun pms-templatize-mit-license ()
  (beginning-of-buffer)
  (replace-string "!!!YEAR!!!" (format-time-string "%Y"))
  (beginning-of-buffer))

(provide 'pms-autoinsert)
