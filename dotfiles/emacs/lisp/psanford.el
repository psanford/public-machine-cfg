(setq org-hide-leading-stars t)
(setq mouse-yank-at-point t)

;; don't split vertically for (display-buffer) (e.g. compilation-mode, diff-mode, etc)
(setq split-height-threshold nil)

(setq emacs-dir "~/projects/public-machine-cfg/dotfiles/emacs/")

(setq company-go-gocode-command "gocode")

(setq x-select-enable-clipboard-manager nil)

(dolist (r `(
             (?e (file . ,(concat emacs-dir "init.el")))
             (?f (file . "~/projects/foo/foo.go"))
             (?h (file . "~/projects/public-machine-cfg/dotfiles/hyprland/hyprland.conf"))
             (?l (file . "~/projects/machine-cfg/log/general.log"))
             (?m (file . "~/projects/public-machine-cfg"))
             (?o (file . "~/projects/nix-cfg/onan/configuration.nix"))
             (?p (file . ,(concat emacs-dir "lisp/psanford.el")))
             (?r (file . "~/projects/public-machine-cfg/dotfiles/river/init"))
             ))
  (set-register (car r) (cadr r)))

(unless (get-register ?j)
  (set-register ?j `(file . ,(concat "~/projects/nix-cfg/" system-name "/configuration.nix"))))

(setq org-default-notes-file "~/.notes.org")

;; work around tramp hanging on startup when tethered to tmobile
;; https://magit.vc/manual/magit/Emacs-245-hangs-when-loading-Magit.html#Emacs-245-hangs-when-loading-Magit
(setq tramp-use-ssh-controlmaster-options nil)

;; don't show tag in status buffer
(require 'magit)
(when (member 'magit-insert-tags-header magit-status-headers-hook)
  (setq magit-status-headers-hook (remove 'magit-insert-tags-header magit-status-headers-hook)))

(setq magit-status-sections-hook
      '(magit-insert-status-headers
        magit-insert-merge-log
        magit-insert-rebase-sequence
        magit-insert-am-sequence
        magit-insert-sequencer-sequence
        magit-insert-bisect-output
        magit-insert-bisect-rest
        magit-insert-bisect-log
        magit-insert-stashes
        magit-insert-untracked-files
        magit-insert-unstaged-changes
        magit-insert-staged-changes
        magit-insert-unpulled-from-upstream
        magit-insert-unpulled-from-pushremote
        magit-insert-unpushed-to-upstream
        magit-insert-unpushed-to-pushremote))

;; don't touch my buffers, magit!
(magit-auto-revert-mode -1)

;; don't write files to trash folder
(setq magit-delete-by-moving-to-trash nil)

(add-hook 'git-commit-mode-hook 'git-commit-turn-on-flyspell)

(add-hook 'markdown-mode-hook
          (lambda () (flyspell-mode 1)))

(add-hook 'erc-mode-hook
          (lambda () (erc-spelling-mode 1)))

;; c completion
;; (global-ede-mode 1)
;; (semantic-mode 1)

(defun pms-small-screen ()
  (interactive)
  (setq nb-font-name "Inconsolata:style=Medium:size=18")
  (setq nb-font-height 135)
  (set-face-attribute 'default nil :height nb-font-height))

(defun pms-big-screen ()
  (interactive)
  (setq nb-font-name "Inconsolata:style=Medium:size=18")
  (setq nb-font-height 60)
  (set-face-attribute 'default nil :height nb-font-height))

(defun pms-small-frame ()
  (interactive)
  (set-face-attribute 'default (selected-frame) :height 135))

(defun pms-insert-with-indentation (str)
  (save-excursion
    (beginning-of-line)
    (unless
        (string-match
         "^\s*$"
         (buffer-substring (line-beginning-position) (line-end-position)))
      (insert "\n")
      (forward-line -1))
    (insert str)
    (indent-for-tab-command)))

(defun pms-normal-screen ()
  (interactive)
  (setq nb-font-height 180)
  (setq nb-font-name "Inconsolata:style=Medium:size=24")
  (set-face-attribute 'default nil :height nb-font-height))

(if (= 800 (display-pixel-height))
    (progn
      (pms-small-screen)
      (when (not (null (font-info nb-font-name)))
        (add-to-list 'default-frame-alist `(font . ,nb-font-name)))
      (set-frame-position (selected-frame) 503 24)
      (set-frame-height (selected-frame) 36))
  (setq nb-font-name "Inconsolata:style=Medium:size=24")
;  (set-frame-position (selected-frame) 20 350)
  (set-frame-height (selected-frame) 58)
  (set-frame-width (selected-frame) 85))

(defun desktop-save-no-prompt ()
  (interactive)
  (when (and (boundp 'desktop-owner) (eq (desktop-owner) (emacs-pid)))
    (desktop-save desktop-dirname)))

(add-hook 'auto-save-hook 'desktop-save-no-prompt)

(setq message-auto-save-directory "~/.emacs_mail/drafts")

(setq eshell-path-env (getenv "PATH"))

;; taken from https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun pms-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive (list (read-string "New name: " (file-name-nondirectory (buffer-file-name)))))
  (let ((name (buffer-name))
	      (filename (buffer-file-name)))
    (if (not filename)
	      (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	        (message "A buffer named '%s' already exists!" new-name)
	      (progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil)))))) ;;

(defalias 'rename-file-and-buffer 'pms-rename-file-and-buffer)

;; taken from https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun pms-move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
  (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t))))

;; (require 'malabar-mode)
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;; fix faces for emacs 24.2.50.1
;; (setq ansi-term-color-vector
;;   [term
;;    term-color-black
;;    term-color-red
;;    term-color-green
;;    term-color-yellow
;;    term-color-blue
;;    term-color-magenta
;;    term-color-cyan
;;    term-color-white])
;; (set-face-foreground 'term-color-blue "RoyalBlue1")
;; (set-face-background 'term-color-blue "RoyalBlue1")

(defun pms-toggle-docker-file ()
  (interactive)
  (let ((docker-host (funcall nb-docker-host-func)))
    (find-file (concat (concat "/ssh:" docker-host ":" buffer-file-name)))))

(setq nb-docker-mode t)
(setq nb-docker-host-func
      #'(lambda ()
          (shell-command-to-string "devshell --ip")))

(defun pms-docker-toggle-test-mode-for-buffer ()
  "Toggle nb-docker-mode for the current buffer only"
  (interactive)
  (set (make-local-variable 'nb-docker-mode) (not nb-docker-mode))
  (message "set local buffer nb-docker-mode to %s" nb-docker-mode))

(require 'tramp)

;; from http://www.columbia.edu/kermit/case26.html
(define-derived-mode kermit-mode
  awk-mode "Kermit"
  "Major Mode for Kermit Scripts"
  (auto-fill-mode 1)
  (setq fill-column 78)
  (setq indent-tabs-mode nil)
  (setq brace-else-brace 1)
  (setq comment-multi-line nil)
  (setq comment-column 40)
  (setq comment-start "\# ")
  (setq comment-end ""))

;; (set-face-attribute 'default nil :family "Source Code Pro" :height 110)
;; (set-face-attribute 'default nil :family "Inconsolata" :height 135)

;; fix vc-annotate -- http://lists.gnu.org/archive/html/emacs-bug-tracker/2013-05/msg00147.html
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=iso" "-C" "-C" (if (> (length rev) 0) rev) "--" name)))

(defun pms-godoc (query)
  "Show go documentation for a query, much like M-x man."
  (interactive "Mgodoc: ")
  (unless (string= query "")
    (set-process-sentinel
     (start-process-shell-command "godoc" (godoc--get-buffer query)
                                  (concat "godoc -ex " query))
     'godoc--buffer-sentinel)
    nil))

;; verilog mode, calm down
(setq verilog-auto-inst-vector nil)
(setq verilog-auto-endcomments nil)
(setq verilog-mode-abbrev-table nil)

(setq tags-table-list '("~/.emacs.d/TAGS"))
(setq tags-table-file nil)

(icomplete-mode -1)
(iswitchb-mode t)
(add-hook 'iswitchb-define-mode-map-hook
          '(lambda ()
             (define-key iswitchb-mode-map " " 'iswitchb-next-match)
             (define-key iswitchb-mode-map "\C-j" 'iswitchb-exit-minibuffer)))

;; make git annotate faster
(defun pms-git-annotate-command (orig-fun &rest args)
  "Replace git annotate command to remove the -C -C flags"
  (let* ((file (nth 0 args))
         (buf (nth 1 args))
         (rev (nth 2 args))
        (name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=iso"  rev "--" name)))

(advice-add 'vc-git-annotate-command :around #'pms-git-annotate-command)

(defun pms-ppjson-region (beg end)
  "Pretty Print json in region"
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t nil t))

(defun pms-uglyjson-region (beg end)
  "Ugly Print json in region"
  (interactive "r")
  (shell-command-on-region beg end "ppjson -ugly" nil t nil t))

(defun pms-validate-yaml ()
  "Validate yaml"
  (interactive)
  (let ((errbuf (get-buffer-create "*yaml errors*"))
        (tmpfile (make-temp-file "emacs-yamlbuddy-check" nil ".yaml"))
        (filename (buffer-file-name))
        (cmd "/home/psanford/projects/go/bin/yamlbuddy"))
    (save-restriction
      (widen)
      (with-current-buffer errbuf
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert cmd " " filename "\n\n"))

      (write-region nil nil tmpfile)
      (if (zerop (apply 'call-process cmd nil errbuf nil (list tmpfile)))
          (progn
            (kill-buffer errbuf))
        (message "yaml parse errors")
        (with-current-buffer errbuf
          (goto-char (point-min))
          (while (search-forward-regexp (concat "^\\(" (regexp-quote tmpfile) "\\):") nil t)
            (replace-match (file-name-nondirectory filename) t t nil 1))
          (compilation-mode)
          (display-buffer errbuf)))
      (delete-file tmpfile))))

(defun pms-yamllint ()
  "Runs yamllint for current package"
  (interactive)
  (when (buffer-file-name)
    (save-buffer))

  (compile (concat "yamllint -c /home/psanford/src/ansible/roles/common_real/files/yamllint.yml " (buffer-file-name) " | cat"))
  (ring-insert pms-previous-test-ring
               (cons
                `(lambda () (pms-yamllint))
                (current-buffer))))

(define-key yaml-mode-map (kbd "<f5>") 'pms-yamllint)

(add-hook 'yaml-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'pms-validate-yaml nil t)))


(defun pms-increment-number-at-point ()
  "Increment number at point"
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun pms-go-if-error-panic ()
  "Insert if error panic snippet"
  (interactive)
  (let ((start (point)))
    (insert "if err != nil {\n panic(err)\n }\n")
    (indent-region start (point))))


(defun pms-random-string ()
  (interactive)
  (insert (pms-random-word) "-" (pms-random-word)))

(defun pms-wordlist ()
  "Get path to dictionary file."
  (let ((wl (getenv "WORDLIST")))
    ;; nixos doesn't have /usr/share/dict/words
    (if wl wl
      "/usr/share/dict/words")))

(defun pms-random-word ()
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (pms-wordlist))
      (goto-char (random (buffer-size)))
      (let ((word (current-word)))
        (if (or (string-match "'" word) (< (length word) 3))
            (pms-random-word)
          word)))))

(defun pms-random-mac ()
  (interactive)
  (insert (format "00:13:37:%02X:%02X:%02X"
                  (random 255)
                  (random 255)
                  (random 255))))

(defun pms-random-ip ()
  (interactive)
  (insert (format "169.254.%d.%d"
                  (random 255)
                  (random 255))))

(defun pms-random-number ()
  (interactive)
  (insert (format "%d" (random 1000))))


(rg-define-search pms-rg-everywhere
  :query point
  :format literal
  :files "everything"
  :dir project)

(global-set-key (kbd "s-s") 'rg-dwim)
(global-set-key (kbd "s-M-s") 'pms-rg-everywhere)

(defun pms-rg-dir (regexp &optional dir)
  "Call ripgrep --vimgrep similarly to rgrep"
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
                                   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
                (dir (read-directory-name "Base directory: "
                                          nil default-directory t)))
           (list regexp dir))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (unless (and dir (file-accessible-directory-p dir))
      (setq dir default-directory))
    (let ((default-directory dir)
          (grep-highlight-matches 'always))
      (compilation-start (format "rg --color always --vimgrep \"%s\"" regexp) 'grep-mode nil regexp))))

(defun pms-rg (regexp)
  "Call ripgrep --vimgrep similarly to rgrep"
  (interactive "MSearch for: ")
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((grep-highlight-matches 'always))
      (compilation-start (format "rg --color always --vimgrep \"%s\"" regexp) 'grep-mode nil regexp))))

(define-compilation-mode pms-rg-files-mode "pms-rg-files"
  "Sets `grep-last-buffer' and `compilation-window-height'."
  (setq grep-last-buffer (current-buffer))

  (set (make-local-variable 'compilation-error-regexp-alist) '(("^\\([^:]+\\):\\([0-9]+\\)$" 1 2)))
  (set (make-local-variable 'compilation-disable-input) t))


(defun pms-rg-files (command)
  "Search for files by filename"
  (interactive
   (progn
     (list (read-shell-command "Run find (like this): " "rg --files | grep " 'pms-rg-files-history))))
  (when command
    (compilation-start (concat command " | sed 's/$/:1/'") 'pms-rg-files-mode nil nil)))

;; never display images, please
(defun display-images-p (&optional display)
  nil)

;; remove img and doc viewer modes from the auto-mode-alist
(rassq-delete-all #'image-mode auto-mode-alist)
(rassq-delete-all #'doc-view-mode auto-mode-alist)
(rassq-delete-all #'doc-view-mode-maybe auto-mode-alist)

;; macos specific
(setq mac-command-modifier 'meta
      mac-option-modifier 'super)

;; enable exec-path-from-shell on macos
(use-package exec-path-from-shell
  :ensure t
  ;; ns for GNUStep and Cocoa
  :if (eq window-system 'ns)
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;; macos specific settings
(when (eq window-system 'ns)
  (setenv "PINENTRY_PROGRAM" "pinentry-mac"))

(let ((claude-path (concat (getenv "HOME") "/projects/claude.el/claude.el"))
      (api-key-path (concat (getenv "HOME") "/.anthropic_api_key")))
  (when (and (file-exists-p claude-path) (file-exists-p api-key-path))
    (require 'claude claude-path)
    (setq claude-api-key
          (string-trim
           (with-temp-buffer
             (insert-file-contents api-key-path)
             (buffer-string))))))
