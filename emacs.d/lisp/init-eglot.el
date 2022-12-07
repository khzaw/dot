(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((typescript-tsx-mode
           scss-mode
           css-mode
           python-mode
           go-mode) . eglot-ensure)
  ;; :hook ((go-mode
  ;;          web-mode
  ;;          js-mode
  ;;          python-mode
  ;;          rust-mode) . eglot-ensure)
  :custom (eglot-autoshutdown t)
  :bind (("C-c e f" . #'eglot-format)
          ("C-c e a" . #'eglot-code-actions)
          ("C-c e i " . #'eglot-code-action-organize-imports))
  :config
  (setq eglot-strict-mode nil)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq completion-category-overrides '((eglot (styles orderless))))

  (defun eglot-capf ()
    (setq-local completion-at-point-functions
      (list (cape-super-capf
              #'eglot-completion-at-point
              #'tempel-expand
              #'cape-file))))

  (defun eglot-actions-before-save ()
    (add-hook 'before-save-hook (lambda ()
                                  (call-interactively #'eglot-format)
                                  (call-interactively #'eglot-code-action-organize-imports))))
  (add-hook 'eglot-managed-mode-hook #'eglot-actions-before-save)
  (add-hook 'eglot-managed-mode-hook #'eglot-capf))

(use-package consult-eglot
  :after (consult eglot vertico))

(provide 'init-eglot)
