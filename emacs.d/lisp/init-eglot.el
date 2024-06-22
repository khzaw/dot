(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((go-mode
          go-ts-mode
          css-mode css-ts-mode
          python-mode python-ts-mode
          markdown-mode
          java-mode java-ts-mode
          tsx-ts-mode typescript-ts-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  :bind (("C-c e e" . #'eglot)
         ("C-c e f" . #'eglot-format)
         ("C-c e a" . #'eglot-code-actions)
         ("C-c e o" . #'eglot-code-action-organize-imports)
         ("C-c e t" . #'eglot-find-typeDefinition)
         ("C-c e i" . #'eglot-find-implementation)
         ("C-c e d" . #'eglot-find-declaration)
         ("C-c e p" . #'eldoc-print-current-symbol-info))
  :config
  (cl-pushnew '((js-mode jsx-mode rjsx-mode typescript-mode tsx-ts-mode) . ("typescript-language-server" "--stdio"))
              eglot-server-programs
              :test #'equal)

  (setq eldoc-echo-area-use-multiline-p t)
  (setq eglot-strict-mode nil)
  ;; (setq eglot-events-buffer-size 0)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq completion-category-overrides '((eglot (styles orderless))))
  ;; (setq eglot-stay-out-of '(eldoc-documentation-strategy))

  (defun eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-expand
                       #'cape-file
                       #'cape-line
                       #'cape-keyword))))

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
  :disabled t
  :straight (:type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(use-package eldoc-box
  :bind (:map eglot-mode-map
         ("C-c e m" . eldoc-box-help-at-point))
  :config
  (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
        (alist-get 'right-fringe eldoc-box-frame-parameters) 8))

(use-package xref
  :straight (:type built-in))


(provide 'init-eglot)
