(defun pms-git-remote-url-to-web-url (url)
  "Convert a git remote url to a web url"

  (setq url (replace-regexp-in-string "\n" "" url))

  (when (string-match "^git@" url)
    ;; replace github.com: with github.com/
    (setq url (replace-regexp-in-string ":" "/" url))
    (setq url (replace-regexp-in-string "^git@" "https://" url)))
  (setq url (replace-regexp-in-string ".git$" "" url))
  url)

(defvar pms-gitlab-urls "gitlab.com"
  "Regex of gitlab urls")

(defun pms-git-browse-file (base-url rev file-path line-num)
  "Browse to file in github, GHE or gitlab repo"
  (let ((url
         (cond
          ((string-match pms-gitlab-urls base-url)
           (concat base-url "/-/blob/" rev "/" file-path "#L" line-num))
          (t (concat base-url "/blob/" rev "/" file-path "#L" line-num)))))
    (message url)
    (browse-url url)))

(defun pms-git-browse-commit (base-url commit-sha)
  "Browse to file in github,GHE or gitlab repo"
  (let ((url
         (cond
          ((string-match pms-gitlab-urls base-url)
           (concat base-url "/-/commit/" commit-sha))
          (t (concat base-url "/commit/" commit-sha)))))
    (message url)
    (browse-url url)))

(defun pms-git-commit-from-log ()
  "Return commit sha of the current log message from vc-log style buffers"
  (save-excursion
    (beginning-of-line)
    (while (and (not (looking-at "commit [a-zA-Z0-9]+"))
                (not (bobp)))
      (forward-line -1))
    (if (looking-at "commit [a-zA-Z0-9]+")
        (buffer-substring (+ (point) (length "commit ")) (line-end-position))
      (error "Failed to find git 'commit sha' line"))))

(defun pms-browse-git ()
  "Open ref in github"
  (interactive)
  (let ((file-path (string-remove-prefix (projectile-project-root) (buffer-file-name)))
        (line-num (format "%d" (line-number-at-pos)))
        (current-rev (string-trim-right (shell-command-to-string "git rev-parse HEAD")))
        (base-url
          (pms-git-remote-url-to-web-url
           (shell-command-to-string "git remote get-url origin"))))

    (cond
     ((eq major-mode 'vc-annotate-mode)
      (progn
        (setq current-rev (car (vc-annotate-extract-revision-at-line)))
        (setq file-path (string-remove-prefix (projectile-project-root) (cdr (vc-annotate-extract-revision-at-line))))
        (pms-git-browse-file base-url current-rev file-path line-num)))
     ((derived-mode-p 'log-view-mode)
      (pms-git-browse-commit base-url (pms-git-commit-from-log)))
     ;; in magit-revision mode (viewing a commit)
     (magit-buffer-revision-hash (pms-git-browse-commit base-url magit-buffer-revision-hash))
     (t (pms-git-browse-file base-url current-rev file-path line-num)))))

(provide 'pms-git)
