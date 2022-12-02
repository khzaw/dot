(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook ((go-mode . eglot-ensure)
          (rust-mode . eglot-ensure))
  :config
  (setq eglot-strict-mode nil)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq completion-category-overrides '((eglot (styles orderless)))))

(use-package consult-eglot
  :defer t
  :after (consult eglot))

(provide 'init-eglot)
