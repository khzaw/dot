(use-package eglot
  :after (evil evil-leader typescript-mode)
  :commands (eglot eglot-ensure)
  :hook ((typescript-tsx-mode
          typescript-mode
          css-mode
          python-mode python-ts-mode
          markdown-mode
          java-mode
          go-mode go-ts-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  :bind (("C-c e f" . #'eglot-format)
         ("C-c e a" . #'eglot-code-actions)
         ("C-c e i " . #'eglot-code-action-organize-imports))
  :config
  (cl-pushnew '((js-mode jsx-mode rjsx-mode typescript-mode typescript-tsx-mode) . ("typescript-language-server" "--stdio"))
              eglot-server-programs
              :test #'equal)
  (setq eglot-strict-mode nil)
  (setq eglot-events-buffer-size 0)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-auto-display-help-buffer nil)
  (setq completion-category-overrides '((eglot (styles orderless))))
  ;; (setq eglot-stay-out-of '(eldoc-documentation-strategy))

  (defun eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-expand
                       #'cape-file))))

  (defun eglot-actions-before-save ()
    (add-hook 'before-save-hook (lambda ()
                                  (when (not (memq major-mode '(typescript-tsx-mode typescript-mode)))
                                    (call-interactively #'eglot-format))
                                  (call-interactively #'eglot-code-action-organize-imports))))
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
    (evil-leader/set-key "gi" 'eglot-find-implementation)
    (evil-leader/set-key "ty" 'eglot-find-typeDefinition)))

(use-package consult-eglot
  :bind (:map eglot-mode-map
         ([remap xref-find-apropos] .  consult-eglot-symbols)))

(use-package flycheck-eglot
  :straight (:type git :repo "intramurz/flycheck-eglot" :host github)
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))

(use-package eglot-booster
  :straight (:type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(provide 'init-eglot)
