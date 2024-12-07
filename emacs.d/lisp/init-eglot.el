(use-package eglot
  :commands (eglot
             eglot-rename
             eglot-format-buffer
             eglot-ensure)
  :hook ((go-mode
          go-ts-mode
          css-mode css-ts-mode
          python-mode python-ts-mode
          markdown-mode
          java-mode java-ts-mode
          tsx-ts-mode typescript-ts-mode)
         . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-report-progress nil) ; Prevent minibuffer spams
  :bind (("C-c e e" . #'eglot)
         ("C-c e f" . #'eglot-format)
         ("C-c e a" . #'eglot-code-actions)
         ("C-c e o" . #'eglot-code-action-organize-imports)
         ("C-c e t" . #'eglot-find-typeDefinition)
         ("C-c e i" . #'eglot-find-implementation)
         ("C-c e d" . #'eglot-find-declaration)
         ("C-c e p" . #'eldoc-print-current-symbol-info))
  :config
  ;; Optimizations
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)

  (setq eglot-extend-to-xref t)

  (cl-pushnew
   '(t
     (js-mode jsx-mode rjsx-mode typescript-mode typescript-ts-mode tsx-ts-mode)
     . ("typescript-language-server" "--stdio"))
   eglot-server-programs
   :test #'equal)

  (cl-pushnew
   '(solidity-mode . ("nomicfoundation-solidity-language-server" "--stdio"))
   eglot-server-programs
   :test #'equal)

  ;; specify explicitly to use orderless for eglot
  (setq completion-category-overrides '((eglot (styles . (orderless flex)))
                                        (eglot-capf (styles . (orderless flex)))))
  ;; enable cache busting
  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (setq eldoc-echo-area-use-multiline-p t)
  (setq eglot-strict-mode nil)
  ;; (setq eglot-events-buffer-size 0)
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

  (defun eglot-actions-before-save ()
    (add-hook 'before-save-hook (lambda ()
                                  (when (not (memq major-mode '(tsx-ts-mode typescript-ts-mode)))
                                    (call-interactively #'eglot-format)
                                    (call-interactively #'eglot-code-action-organize-imports)))))
  (add-hook 'eglot-managed-mode-hook #'eglot-actions-before-save)
  (add-hook 'eglot-managed-mode-hook #'eglot-capf)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose)))

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

(use-package consult-eglot
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] .  consult-eglot-symbols)))

(use-package flycheck-eglot
  :straight (:type git :repo "intramurz/flycheck-eglot" :host github)
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))

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
        (flymake-eslint-enable))))

  (defun khz/configure-eslint-with-flymake ()
    (when (or (eq major-mode 'tsx-ts-mode)
              (eq major-mode 'typescript-ts-mode)
              (eq major-mode 'typescriptreact-mode))
      (khz/use-local-eslint)))

  (add-hook 'eglot-managed-mode-hook #'khz/use-local-eslint)

  ;; (add-hook 'js-ts-mode-hook #'khz/use-local-eslint)
  )

(use-package eldoc-box
  :bind (:map eglot-mode-map
         ("C-c e m" . eldoc-box-help-at-point))
  :config
  (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
        (alist-get 'right-fringe eldoc-box-frame-parameters) 8))

(use-package xref
  :straight (:type built-in))

(use-package dape
  :config
  (setq dape-buffer-window-arrangement 'right)

  ;; To not display info and/or buffers on startup
  (remove-hook 'dape-stopped-hook 'dape-info)
  (remove-hook 'dape-start-hook 'dape-repl)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root))

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
  :after eglot
  :straight (:type git :host github :repo "Gavinok/eglot-codelens")
  :config
  (eglot-codelens-mode +1))

(use-package eglot-menu
  :straight (eglot-menu
             :repo "KarimAziev/eglot-menu"
             :type git
             :host github)
  :commands (eglot-menu-reconnect))

(provide 'init-eglot)
