(require 'cl-macs)

(defun pms-calc-eval-region (beg end)
  "Eval the arithmetic expression in the region and replace it with the result"
  (interactive "r")
  (let ((val (calc-eval (buffer-substring beg end))))
    (delete-region beg end)
    (insert val)))

(defun pms-calc-eval-line ()
  "Eval the arithmetic expression on the current line and insert it at the beginning"
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (goto-char (point-min))
      (when (re-search-forward "==" nil t)
        (delete-region (point-min) (point)))
      (goto-char (point-min))
      (just-one-space 0)
      (let ((val (calc-eval (buffer-substring (point-min) (point-max)))))
        (beginning-of-line)
        (insert val " == ")))))

(defun pms-hexify-region (beg end)
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((hex (format "\\x%x" (char-after))))
        (delete-char 1)
        (insert hex)))))

(defun pms-decode-hex-string (hex-string)
  (let ((res nil))
    (dotimes (i (/ (length hex-string) 2) (apply #'concat (reverse res)))
      (let ((hex-byte (substring hex-string (* 2 i) (* 2 (+ i 1)))))
        (push (format "%c" (string-to-number hex-byte 16)) res)))))

(defun pms-decode-hex-region (beg end)
  (interactive "r")
  (let ((decoded (pms-decode-hex-string (buffer-substring beg end))))
    (delete-region beg end)
    (insert decoded)))

(defvar pms-previous-test-ring (make-ring 5))
(defun pms-run-last-test (&optional n)
  "Run most recently run test again"
  (interactive "P")
  (when (buffer-file-name)
    (save-buffer))
  (setq n
        (cond
         ((null n) 0)
         ((listp n) 1)
         (1 n)))
  (let ((prev (ring-ref pms-previous-test-ring n)))
    (if (> n 0)
        (ring-insert pms-previous-test-ring prev))
    (let ((command (car prev))
          (buffer (cdr prev)))
      (switch-to-buffer buffer)
      (funcall command))))


(defun pms-hexify-url (beg end)
  "Unescapes current region"
  (interactive "r")
  (let ((text (url-hexify-string (buffer-substring beg end))))
    (delete-region beg end)
    (insert text)))

(defun pms-unhex-url (beg end)
  "Unescapes current region"
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring beg end))))
    (delete-region beg end)
    (insert text)))

(defun pms-unescape-uri ()
  "Unescapes current region"
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "%" 'nil t)
      (delete-char -1)
      (let ((hex (buffer-substring (point) (+ 2 (point)))))
        (delete-char 2)
        (insert (char-to-string (string-to-number hex 16)))))))

(defun pms-escape-uri ()
  "Escapes current region"
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (let ((escaped (url-encode-url (buffer-substring (point-min) (point-max)))))
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (insert escaped))))

(defun pms-random-number ()
  "Insert a random number"
  (interactive)
  (insert (format "%d" (random 1000))))

(defun pms-random-uuid ()
  "Insert a uuid"
  (interactive)
  (insert (pms-chomp (shell-command-to-string "uuidgen"))))

;; Taken from http://www.emacswiki.org/emacs/ToggleWindowSplit
(defun pms-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun pms-chomp (s)
  (replace-regexp-in-string "[ \t\n\r]+$" "" s))

(defun pms-split-camel (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun pms-camelcase  (s) (mapconcat 'capitalize (pms-split-camel s) ""))
(defun pms-underscore (s) (mapconcat 'downcase   (pms-split-camel s) "_"))

(defun pms-camelscore (s)
  (cond ((string-match-p "_" s)	(pms-camelcase s))
        ((string-match-p "^[a-z]" s) (pms-camelcase s))
        (t (pms-underscore s))))

(defun pms-camelscore-word-at-point ()
  (interactive)
  (let* ((case-fold-search nil)
         (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
         (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
         (txt (buffer-substring beg end))
         (cml (pms-camelscore txt)) )
    (if cml (progn (delete-region beg end) (insert cml))) ))

(defun pms-update-buffers ()
  "Reload all open buffers"
  (interactive)
  (let ((list (buffer-list)) buffer errmesg)
    (cl-loop for buffer in (buffer-list) do
          (cond ((eq 'dired-mode (buffer-local-value 'major-mode (get-buffer buffer)))
                 (set-buffer buffer)
                 (condition-case nil
                     (revert-buffer t t t)
                   (error nil)))
                ((and (not (string-match "\\*" (buffer-name buffer)))
                      (buffer-file-name buffer)
                      (file-exists-p (buffer-file-name buffer)))
                 (if (and (not (verify-visited-file-modtime buffer))
                          (buffer-modified-p buffer))
                     (setq errmesg (concat errmesg (format "Buffer '%s' has file and buffer changes!\n" buffer)))
                   (if (not (verify-visited-file-modtime buffer))
                       (progn
                         (message "%s %s %s" (buffer-name buffer) (verify-visited-file-modtime buffer) (buffer-modified-p buffer))
                         (set-buffer buffer)
                         (message "Refreshing %s" (buffer-file-name buffer))
                         (revert-buffer t t t)))))))
    (message "%s" (or errmesg "Done"))))

(defun pms-wget (url)
  "Download file to the working directory"
  (interactive "sURL: ")
  (switch-to-buffer (get-buffer-create "*wget*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (call-process "wget" nil t t url)
  (setq buffer-read-only t))

(defun pms-rfc3339-time ()
  "Insert current rfc3339 time"
  (interactive)
  (insert "\"" (format-time-string "%Y-%m-%dT%H:%M:%S%:z") "\"" ))

(defun pms-epoch-time ()
  "Insert current unix epoch time"
  (interactive)
  (insert (format "%d" (time-to-seconds))))

(defun pms-sort ()
  "Sort lines between [ and ]"
  (interactive)
  (let ((beg) (end) (orig (point)))
    (save-excursion
      (search-backward "[")
      (forward-line)
      (setq beg (point))
      (search-forward "]")
      (setq end (point)))
    (sort-lines nil beg end)
    (goto-char orig)))

(defun pms-save-package-versions ()
  "Save currently installed package version to a file"
  (let ((version-info
           (format "(\"%s\" %s)\n" (format-time-string "%Y-%m-%dT%H:%M:%S%:z")
                   (mapcar
                    (lambda (pkg)
                      `(,pkg ,(package-desc-version
                               (cadr (assq pkg package-alist)))))
                    package-activated-list))))
    (write-region version-info nil (concat dotfiles-dir ".pms-package-version-history") 'append)))

(defun pms-list-package-versions ()
  "Show currently installed packages"
  (interactive)
  (message "(\"%s\" %s)\n" (format-time-string "%Y-%m-%dT%H:%M:%S%:z")
           (mapcar
            (lambda (pkg)
              `(,pkg ,(package-desc-version
                       (cadr (assq pkg package-alist)))))
            package-activated-list)))

(defun pms-wormhole-send-region (beg end)
  "Send region via wormhole"
  (interactive "r")
  (setq pms--wormhole-process-output "")
  (let* ((buffer-name "*wormhole-william output*")
         (buffer (get-buffer-create buffer-name))
         (text (buffer-substring beg end))
         (pms--wormhole-process-send-output "")
         (ww))
    (with-current-buffer buffer
      (erase-buffer))
    (setq ww (start-process
              "wormhole-william"
              buffer
              "wormhole-william"
              "send" "--text" "-"))
    (set-process-filter ww 'pms--wormhole-process-send-output)
    (process-send-string ww text)
    (process-send-eof ww)
    (while (accept-process-output ww)))
  (setq pms--wormhole-process-send-output "")
  (message "wormhole done"))

(defvar pms--wormhole-process-output "")

(defun pms--wormhole-process-send-output (proc output)
  "Output filter looking for wormhole send code."
  (setq pms--wormhole-process-output
        (concat pms--wormhole-process-output output))
  (let ((match (string-match "^Wormhole code is: [0-9]+-.+-.+$" pms--wormhole-process-output)))
    (when match
      (message "%s" (match-string 0 pms--wormhole-process-output)))))

(defun pms-wormhole-recv (code)
  (interactive "sCode: ")
  (insert (pms-chomp (shell-command-to-string (concat "wormhole-william recv " code)))))

(defun pms-shrug ()
  "Insert shrug"
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

(defun pms-remove-ansi-color ()
  "Remove ansi color control codes from buffer"
  (interactive)
  (save-excursion
    (ansi-color-filter-region (point-min) (point-max))))

(defun pms-insert-buffer-name ()
  "Insert buffer name"
  (interactive)
  (insert (buffer-name)))

(defun pms-insert-file-name ()
  "Insert file name"
  (interactive)
  (insert (buffer-file-name)))

(defun pms-timeparse (timestamp)
  "Invoke timeparse on timestamp"
  (interactive
   (list (read-string (format "Parse time (%s): "
                              (if (use-region-p)
                                  (buffer-substring-no-properties (region-beginning) (region-end))
                                (thing-at-point 'word t)))
                      nil nil
                      (if (use-region-p)
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (thing-at-point 'word t)))))
  (when (and (stringp timestamp) (not (string-empty-p timestamp)))
    (compilation-start (format "timeparse \"%s\"" timestamp))))

(provide 'pms-funcs)
