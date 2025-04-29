;; -*- lexical-binding: t; -*-

(use-package eglot
  :straight (:host github :repo "joaotavora/eglot" :tag "v1.18")
  :commands (eglot
             eglot-rename
             eglot-format-buffer
             eglot-ensure)
  :hook
  ((go-mode go-ts-mode) . eglot-ensure)
  ((css-mode css-ts-mode) . eglot-ensure)
  ((python-mode python-ts-mode) . eglot-ensure)
  (markdown-mode . eglot-ensure)
  ((java-mode java-ts-mode) . eglot-ensure)
  ((tsx-ts-mode typescript-ts-mode) . eglot-ensure)
  (tuareg-mode . eglot-ensure)
  (terraform-mode . eglot-ensure)
  ((yaml-mode yaml-ts-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-report-progress nil) ; Prevent minibuffer spams
  :bind (:map eglot-mode-map
              ("C-c e e"   . #'eglot)
              ("C-c e f"   . #'eglot-format)
              ("C-c e a"   . #'eglot-code-actions)
              ("C-c e o"   . #'eglot-code-action-organize-imports)
              ("C-c e t"   . #'eglot-find-typeDefinition)
              ("C-c e i"   . #'eglot-find-implementation)
              ("C-c e d"   . #'eglot-find-declaration)
              ("C-c e p"   . #'eldoc-print-current-symbol-info)
              ("C-c e r"   . #'eglot-rename)
              ("C-c e h c" . #'eglot-hierarchy-call-hierarchy)
              ("C-c e h t" . #'eglot-hierarchy-type-hierarchy))
  :config

  (defvar khz/eglot-server-configs
    ;; Define a mapping of major modes to language server configurations
    '((go-mode go-ts-mode
               :gopls
               (:hints (:parameterNames t
                                        :rangeVariableTypes t
                                        :functionTypeParameters t
                                        :assignVariableTypes t
                                        :compositeLiteralFields t
                                        :compositeLiteralTypes t
                                        :constantValues t)
                       :semanticTokens t
                       :usePlaceholders t
                       :completeUnimported t
                       :matcher "Fuzzy"
                       :deepCompletion t
                       :completionBudget "100ms"
                       :maxCompletionItems 50
                       :gofumpt t))
      (tsx-ts-mode typescript-ts-mode
                   :typescript-language-server (:inlayHints (:parameterNames "all")))

      ;; (rust-mode rust-ts-mode
      ;;  :rust-analyzer (:checkOnSave (:command "clippy")))
      )
    "A list of (MODES SERVER CONFIG) entries for Eglot workspace configurations.
MODES is a list of major modes, SERVER is the language server keyword,
and CONFIG is the configuration plist for that server.")

  (defun khz/update-eglot-workspace-config ()
    "Update `eglot-workspace-configuration' for the current major mode."
    (let ((config (copy-sequence eglot-workspace-configuration))) ;; Avoid mutating global state
      (dolist (entry khz/eglot-server-configs)
        (pcase entry
          (`(,modes ,server ,server-config)
           (when (if (listp modes) (memq major-mode modes) (eq major-mode modes))
             (setq config (plist-put config server server-config))
             (setq-local eglot-workspace-configuration config)
             (cl-return))))))) ;; Exit early once matched

  (add-hook 'eglot-managed-mode-hook #'khz/update-eglot-workspace-config)

  (defvar khz/current-eldoc-symbol nil
    "Stores the symbol for which the Eldoc doc buffer was last toggled in the current buffer.")

  (defun khz/toggle-eldoc-doc-buffer ()
    "Toggle Eldoc documentation buffer for the symbol at point."
    (interactive)
    (let* ((current-symbol (or (thing-at-point 'symbol t) ""))
           (buffer-showing (get-buffer-window "*eldoc*"))
           (last-symbol (buffer-local-value 'khz/current-eldoc-symbol (current-buffer))))
      (if (and buffer-showing (string= current-symbol last-symbol))
          (quit-windows-on "*eldoc*")
        (setq-local khz/current-eldoc-symbol current-symbol)
        (eldoc-doc-buffer t))))

  (with-eval-after-load 'evil
    (evil-define-key 'normal eglot-mode-map
      "K" #'khz/toggle-eldoc-doc-buffer))


  ;; Performance optimizations
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)
  (setq eglot-extend-to-xref t)
  (setq eglot-events-buffer-size 0)

  (dolist (server-programs
           '(((js-mode jsx-mode rjsx-mode typescript-mode typescript-ts-mode tsx-ts-mode)
              . ("typescript-language-server" "--stdio"))
             (solidity-mode . ("nomicfoundation-solidity-language-server" "--stdio"))
             (astro-ts-mode . ("astro-ls" "--stdio"
                               :initializationOptions
                               (:typescript (:tsdk "./node_modules/typescript/lib"))))
             (yaml-mode . ("yaml-language-server" "--stdio"))
             ((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio"))))
    (add-to-list 'eglot-server-programs server-programs))

  ;; enable cache busting
  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (setq eldoc-echo-area-use-multiline-p t)
  (setq eldoc-echo-area-prefer-doc-buffer t)
  (setq eglot-strict-mode nil)
  (setq completion-category-defaults nil)
  (setq eglot-confirm-server-initiated-edits nil)
  ;; (setq eglot-stay-out-of '(eldoc-documentation-strategy))

  (defun eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       ;; #'codeium-completion-at-point
                       #'tempel-expand
                       #'cape-line))))

  ;; specify explicitly to use orderless for eglot

  (setq completion-category-overrides '((eglot (styles . (orderless flex)))
                                        (eglot-capf (styles . (orderless flex)))))

  (defun khz/eglot-actions-before-save ()
    "Run Eglot format and organize imports before saving, except in TypeScript modes."
    (when (eglot-managed-p)
      (unless (memq major-mode '(tsx-ts-mode typescript-ts-mode astro-ts-mode))
        (condition-case err
            (progn
              (eglot-format-buffer)
              (eglot-code-actions nil nil "source.organizeImports" t))
          (error
           (message "Eglot pre-save actions failed: %s" err))))))

  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'khz/eglot-actions-before-save nil t)))
  (add-hook 'eglot-managed-mode-hook #'eglot-capf)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)))

  ;; (load (expand-file-name "lisp/init-flycheck-eglot.el" user-emacs-directory))

  (progn
    (evil-leader/set-key
      "gi" 'eglot-find-implementation
      "ty" 'eglot-find-typeDefinition
      "ed" 'eglot-find-declaration)))

(use-package eglot-booster
  :straight (:type git :host github :repo "jdtsmith/eglot-booster")
  :if (executable-find "emacs-lsp-booster")
  :after eglot
  :config (eglot-booster-mode))

;; Get hierarchy
(use-package eglot-hierarchy
  :commands (eglot-hierarchy-call-hierarchy eglot-hierarchy-type-hierarchy)
  :straight (:type git :host github :repo "dolmens/eglot-hierarchy"))

(use-package consult-eglot
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] .  consult-eglot-symbols))
  :config

  (defun khz/consult-eglot-workspace-symbols ()
    "Search workspace symbols with Consult-Eglot."
    (interactive)
    (consult-eglot-symbols nil t))
  (bind-key "C-c e w" #'khz/consult-eglot-workspace-symbols eglot-mode-map))

(use-package consult-eglot-embark
  :after (embark consult-eglot)
  :config (consult-eglot-embark-mode))

(use-package eldoc-box
    :bind (:map eglot-mode-map
                ("C-c e m" . eldoc-box-help-at-point))
    :custom
    (eldoc-box-lighter nil)
    (eldoc-box-only-multi-line t)
    (eldoc-box-clear-with-C-g t)
    :custom-face
    (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
    (eldoc-box-body ((t (:inherit tooltip))))
    :config
    (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
          (alist-get 'right-fringe eldoc-box-frame-parameters) 8))

(use-package flycheck-eglot
  :straight (:type git :repo "flycheck/flycheck-eglot" :host github)
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive t))

;; show breadcrumbs; not on by default
(use-package breadcrumb
  :straight (:repo "joaotavora/breadcrumb" :host github :type git))

(use-package flymake-eslint
  :config
  (setq flymake-eslint-prefer-json-diagonistics t)

  (defun khz/use-local-eslint ()
    "Set proejct's `node_modules' binary eslint as first priority.
If nothing is found, keep the default value flymake-eslint set or your override
of `flymake-eslint-executable-name.'"
    (interactive)
    (let* ((root (locate-dominating-file (buffer-file-name) "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/.bin/eslint" root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flymake-eslint-executable-name eslint)
        (message (format "Found local eslint. Setting: %s" eslint))
        (unless flymake-mode (flymake-eslint-enable)))))

  (defun khz/configure-eslint-with-flymake ()
    (when (or (eq major-mode 'tsx-ts-mode)
              (eq major-mode 'typescript-ts-mode)
              (eq major-mode 'typescriptreact-mode))
      (khz/use-local-eslint)))

  (add-hook 'eglot-managed-mode-hook #'khz/use-local-eslint)

  ;; (add-hook 'js-ts-mode-hook #'khz/use-local-eslint)
  )


(use-package dape
  :config
  (setq dape-buffer-window-arrangement 'right)

  ;; To not display info and/or buffers on startup
  (remove-hook 'dape-stopped-hook 'dape-info)
  (remove-hook 'dape-start-hook 'dape-repl)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root)

  (defun khz/eglot-dape-debug-at-point ()
    "Jump to definition and start Dape debugging."
    (interactive)
    (call-interactively #'eglot-find-definition)
    (dape))

  (with-eval-after-load 'eglot
    (bind-key "C-c e D" #'khz/eglot-dape-debug-at-point eglot-mode-map))

  ;; (setq dape-configs
  ;;       (append dape-configs
  ;;               '((go-dlv modes (go-mode go-ts-mode)
  ;;                         :command "dlv dap"
  ;;                         :port 38697)
  ;;                 (node modes (tsx-ts-mode typescript-ts-mode)
  ;;                       :command "node --inspect"))))
  )

(use-package sideline-eglot
  :straight (:type git :host github :repo "emacs-sideline/sideline-eglot")
  :init
  (setq sideline-backends-right '(sideline-eglot)))

(use-package eglot-inactive-regions
  :straight (:type git :host github :repo "fargiolas/eglot-inactive-regions")
  :custom
  (eglot-inactive-regions-style 'darken-foreground)
  (eglot-inactive-regions-opacity 0.4)
  :config
  (eglot-inactive-regions-mode 1))

(use-package eglot-codelens
  :straight (:type git :host github :repo "Gavinok/eglot-codelens")
  :after eglot
  :hook (eglot-managed-mode . eglot-codelens-mode))

(use-package eglot-menu
  :straight (eglot-menu
             :repo "KarimAziev/eglot-menu"
             :type git
             :host github)
  :commands (eglot-menu-reconnect))

(provide 'init-eglot)
