(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(when (eq window-system 'x)
  ;; don't display builtin emacs window decoration
  (set-frame-parameter nil 'undecorated t)
  (add-to-list 'default-frame-alist '(undecorated . t)))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq lisp-dir (concat dotfiles-dir "lisp/"))
(setq thirdparty-dir (concat lisp-dir "thirdparty/"))

(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path thirdparty-dir)
;; (add-to-list 'load-path (concat lisp-dir "elpa-to-submit"))
;; (add-to-list 'load-path (concat lisp-dir "elpa-to-submit/themes"))

(setq autoload-file (concat lisp-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'package)

;; this advice causes install-package to package-refresh-contents the first time
;; it actually installs a package to avoid stale cache errors
(defun my-package-install-refresh-contents (&rest args)
  (package-refresh-contents)
  (advice-remove 'package-install 'my-package-install-refresh-contents))
(advice-add 'package-install :before 'my-package-install-refresh-contents)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Thirdparty packages to install
(setq thirdparty-packages (list
                                 ;;starter-kit defaults
                                 'yaml-mode
                                 'gist

                                 'inf-ruby
                                 'js2-mode
                                 'smex
                                 'rainbow-mode
                                 'company
                                 'web-mode
                                 'protobuf-mode
                                 'use-package
                                 'color-theme
                                 'magit
                                 'projectile
                                 'puppet-mode
                                 'yasnippet
                                 ))


;; refresh package list if it is not already available
(when (not package-archive-contents) (package-refresh-contents))

;; install packages from the list that are not yet installed
(dolist (pkg thirdparty-packages)
    (when (and (not (package-installed-p pkg)) (assoc pkg package-archive-contents))
        (package-install pkg)))

(require 'git-time-machine)
(require 'json-error)
(require 'pms-packages)
(require 'pms-settings)
(require 'pms-funcs)
(require 'pms-term)
(require 'pms-bindings)
(require 'pms-autoinsert)

(require 'pms-git)
(require 'pms-go)
(require 'pms-rust)

(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat lisp-dir system-name ".el")
      user-specific-config (concat lisp-dir user-login-name ".el")
      user-specific-dir (concat lisp-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

(add-to-list 'custom-theme-load-path lisp-dir)
(load-theme 'nearbuy t)

(pms-save-package-versions)
