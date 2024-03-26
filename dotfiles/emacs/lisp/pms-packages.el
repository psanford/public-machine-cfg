(use-package rg
  :ensure t
  :bind (()
         :map rg-mode-map
         ("w" . wgrep-change-to-wgrep-mode))
  :config (progn
            (rg-enable-default-bindings "\M-s")
            (setq rg-command-line-flags '("-M 256" "-g '!node_modules/*'"))
            (setq rg-default-alias-fallback "everything")
            (setq rg-group-result nil)))

            ;; ;; make searching in .js or .hbs also search the other
            ;; (setq rg-custom-type-aliases
            ;;       '(("hbsjs" . "*.hbs *.js *.json")))))

(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode yas-reload-all)
  :delight yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package lsp-mode
  :ensure t
  ;; uncomment to enable gopls http debug server
  :custom (lsp-gopls-server-args '("-debug" "127.0.0.1:0" "-rpc.trace" "-v" "-logfile" "/tmp/gopls.log"))
  ;; :load-path "/home/psanford/projects/thirdparty/lsp-mode"
  :init
  ;; this makes xref-find-references (<M-?>) use identifer at point instead
  ;; of prompting. this makes it usable with lsp-mode.
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references))
  :config (progn
            (setq lsp-clients-go-library-directories '("/usr" "/home/psanford/lib" "/nix" "/opt" "/home/psanford/.cache/gopath"))
            (setq lsp-enable-links nil)
            (setq lsp-enable-indentation nil)
            (setq lsp-gopls-hover-kind "FullDocumentation")
            (setq lsp-headerline-breadcrumb-enable nil)
            ;; use flycheck, not flymake
            (setq lsp-prefer-flymake nil)))
            ;; ;; don't show the 'test code lense
            ;; (assq-delete-all 'test lsp-go-codelenses))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config (progn
            ;; disable inline documentation
            (setq lsp-ui-sideline-enable nil)
            ;; disable showing docs on hover at the top of the window
            (setq lsp-ui-doc-enable nil))
  )


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package company
  :ensure t
  :config (progn
            ;; don't add any dely before trying to complete thing being typed
            ;; the call/response to gopls is asynchronous so this should have little
            ;; to no affect on edit latency
            (setq company-idle-delay 0)
            ;; start completing after a single character instead of 3
            (setq company-minimum-prefix-length 1)
            ;; align fields in completions
            (setq company-tooltip-align-annotations t)
            ;; disable default dabbrev completion in company mode
            ;; otherwise this tries to complete in comment blocks
            (delete 'company-dabbrev company-backends)
            ;; disable icons
            (setq company-format-margin-function nil)
            )
  )

;; optional package to get the error squiggles as you edit
(use-package flycheck
  :ensure t)

(use-package rjsx-mode
  :ensure t)

(use-package dumb-jump
  :ensure t
  :config (progn
            (dumb-jump-mode)))

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'")

(use-package rust-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package toggle-quotes
  :ensure t)

(use-package hack-mode
  :ensure t)

(use-package hide-lines
  :ensure t)

(use-package go-impl
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package terraform-mode
  :ensure t
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package go-tag
  :ensure t)

(use-package nix-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package bpftrace-mode
  :ensure t)

(use-package ellama
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "codellama" :embedding-model "codellama")))

(use-package editorconfig
  :ensure t)

(provide 'pms-packages)
