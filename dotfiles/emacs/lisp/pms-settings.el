(setq whitespace-style '(lines-tail))

(blink-cursor-mode nil)

(setq uniquify-buffer-name-style 'post-forward)

;; disable for tiling window managers
(setq frame-inhibit-implied-resize t)

; disable alarms completely
(setq ring-bell-function 'ignore)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

(setq-default save-place t)
(setq save-place-file "~/.places.emacs.sav")

(setq line-move-visual nil)
(setq kill-whole-line t)

(icomplete-mode t)

(setq-default indent-tabs-mode nil
              c-default-style "k&r"
              indent-level 2
              c-indent-level 2
              css-indent-offset 2
              c-basic-offset 2
              sh-basic-offset 2
              sh-indentation 2
              tab-width 2 ;for external files with \t
              js-indent-level 2
              cperl-indent-level 2)

(setq cperl-under-as-char t
      cperl-indent-parens-as-block nil
      cperl-continued-statement-offset 2
      cperl-brace-offset 0
      cperl-close-paren-offset -2
      cperl-electric-parens nil
      cperl-autoindent-on-semi t
      cperl-auto-newline nil
      cperl-auto-newline-after-colon nil
      cperl-auto-newline-after-brace nil
      cperl-electric-linefeed nil
      cperl-electric-lbrace-space nil
      cperl-break-one-line-blocks-when-indent nil
      cperl-fix-hanging-brace-when-indent nil
      cperl-use-syntax-table-text-property t
      cperl-use-syntax-table-text-property-for-tags t
      cperl-invalid-face nil)


(projectile-mode)
(setq projectile-project-search-path '("~/projects/" "~/src/")
      projectile-switch-project-action 'projectile-dired
      projectile-completion-system 'ido)

; show lines and columns
(line-number-mode t)
(column-number-mode t)


;; deal with long lines better
(global-so-long-mode 1)
;; from https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

; short answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq show-trailing-whitespace t)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'tex-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'magit-status-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'nxml-mode-hook (lambda ()
                            (setq show-trailing-whitespace t)
                            (rng-set-vacuous-schema) ; shutup "using vacuous schema"
                            ))

; modes
(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("bash" . sh-mode))
(add-to-list 'auto-mode-alist '("emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("elisp" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("/tmp/diff$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pl$" . cperl-mode))

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

;; lisp debug print entire expression
(setq eval-expression-print-length nil)
(setq print-length nil)

; font lock in all major modes
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; always end a file with a newline
(setq require-final-newline t)

; don't want any startup message
(setq inhibit-startup-message t)

; tail compilation buffers
(setq compilation-scroll-output t)

; disable popup dialog box
(setq use-dialog-box nil)

; find-grep ignore gitfiles
(require 'grep)
(grep-apply-setting 'grep-find-command '("find . -path '*/.git' -prune -o -name development.log -prune -o -name test.log -prune -o -type f -exec grep -nH -e  {} /dev/null \\;" . 116))

(grep-apply-setting 'grep-find-command nil)

; scroll settings
(setq scroll-conservatively 500
      scroll-preserve-screen-position t)

; tab completion
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; (require 'smart-tab)
;; (global-smart-tab-mode 1)
(setq hippie-expand-try-functions-list (list
  'try-expand-dabbrev-visible
  'try-expand-dabbrev
  'try-expand-dabbrev-all-buffers
  'try-expand-dabbrev-from-kill
  'try-complete-file-name-partially
  'try-complete-file-name
))

;; diff-mode post emacs27
;; don't try to fontify diff buffers based on the mode
;; just +/- coloring please
(setq diff-refine nil)
(setq diff-font-lock-syntax nil)
(setq diff-font-lock-prettify t)

;; Use fake xdg-open for links
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "~/bin/xdg-open")

(setq smart-tab-using-hippie-expand t)
(setq smart-tab-disabled-major-modes '(term-mode inf-ruby-mode org-mode eshell-mode python-mode))

(defvar delete-trailing-whitespace-on-save t)
(defun toggle-buffer-delete-trailing-whitespace ()
  (interactive)
  (set
   (make-local-variable 'delete-trailing-whitespace-on-save)
   (not delete-trailing-whitespace-on-save))
  (message "delete whitespace %s"
           (if delete-trailing-whitespace-on-save
               "enabled"
             "disabled")))

(add-hook 'before-save-hook (lambda ()
                              (if delete-trailing-whitespace-on-save
                                  (delete-trailing-whitespace))))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

; make isearch put you at the start of the search, not the end
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (if (and isearch-success isearch-forward isearch-other-end)
      (goto-char isearch-other-end)))

;; highlight commit long lines in magit
(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (whitespace-mode 1)
            (make-local-variable 'whitespace-style)
            (setq whitespace-style '(lines))))

(setq vc-git-print-log-follow t)

;; remember previous open buffers
(setq desktop-dirname "~/.emacs.d/")
;; (desktop-save-mode 1)

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)
(setq ibuffer-saved-filter-groups
      '(("farsell"
         ("emacs" (filename . ".el$"))
         ("devtools" (filename . "/devtools/"))
         ("machine-cfg" (filename . "/machine-cfg/"))
         ("src" (filename . "/src/"))
         ("projects" (filename . "/projects/"))
         ("emacs buffers" (or
                             (name . "\\*Messages\\*")
                             (name . "\\*scratch\\*")
                             (name . "\\*gopls.*")))
         ("magit" (name . "^\\*magit.*\\*$"))
         ("irc" (mode . erc-mode)))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "farsell")))

; turn off new delete behavior
(setq delete-active-region nil)

; use ssh instead of stupid scpx mode for tramp
; this should work a lot better with ssh connection sharing
(setq tramp-default-method "ssh")

;; ignore node_modules directory when running rgrep or find-grep
(when (not (member "node_modules" grep-find-ignored-directories))
  (push "node_modules" grep-find-ignored-directories))

;; ignore build directory when running rgrep or find-grep
(when (not (member "build" grep-find-ignored-directories))
  (push "build" grep-find-ignored-directories))

(let ((ignore-files
       '("terraform.tfstate" "terraform.tfstate.backup" "*.min.js"
         "*.js.map"
         )))
  (dolist (file ignore-files)
    (when (not (member file grep-find-ignored-files))
      (push file grep-find-ignored-files))))

;; web mode settings
(setq web-mode-engines-alist
      '(("ctemplate"    . "\\.html\\'")
        ("ctemplate"    . "\\.html.erb\\'")))
(setq web-mode-markup-indent-offset 2)

(electric-indent-mode 't)

; typo correction
(setq default-abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
                                            ("neabruy" "nearbuy" nil 0)
                                            ("naerbuy" "nearbuy" nil 0)
                                            ("nerabuy" "nearbuy" nil 0)
                                            ))

;; enable default-disabled functions
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(server-start)

(with-eval-after-load 'ispell
  (setq ispell-program-name (executable-find "hunspell")))

(custom-set-variables
 '(sentence-end-double-space nil) ;; keep fill-paragraph from adding double-space after periods
 '(logstash-indent 2)
 '(git-commit-summary-max-length 50))

(dir-locals-set-class-variables
 'nate-brown-js
 '((js2-mode
    (js2-basic-offset . 4)
    (js2-strict-missing-semi-warning))
   (web-mode
    (web-mode-markup-indent-offset . 4))))

(dir-locals-set-directory-class
 "/home/psanford/src/alertcenter" 'nate-brown-js)
(dir-locals-set-directory-class
 "/home/psanford/src/credstore" 'nate-brown-js)
(dir-locals-set-directory-class
 "/home/psanford/src/streamstash" 'nate-brown-js)

(dir-locals-set-class-variables
 'gio-android
 '((go-mode
    (lsp-go-build-flags . ["-tags=android"]))))

(dir-locals-set-directory-class
 "/home/psanford/projects/wormhole-william-gio" 'gio-android)

(setq org-todo-keywords
      '((sequence "TODO" "INPROGRESS" "DONE")))

(add-hook 'js-mode-hook
          (lambda ()
            (when (string-suffix-p ".json" (buffer-file-name))
              (json-error-mode t))))

(setq native-comp-async-report-warnings-errors nil)

(provide 'pms-settings)
