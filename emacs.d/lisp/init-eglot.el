(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook ((go-mode . eglot-ensure)
          (rust-mode . eglot-ensure)
          (typescript-mode . eglot-ensure)
          (python-mode . eglot-ensure))
  :config
  (setq eglot-strict-mode nil)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq completion-category-overrides '((eglot (styles orderless))))
  :custom
  (eglot-autoshutdown t))

(use-package consult-eglot
  :defer t
  :after (consult eglot))

(provide 'init-eglot)
