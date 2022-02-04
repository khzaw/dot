
(use-package python-mode
  :config (setq python-indent-offset 4))

(use-package pyvenv
  :after python
  :config (pyvenv-mode t))

(use-package lsp-python-ms
  :after lsp-mode
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred))))
(use-package poetry
  :after python
  :init
  (setq poetry-tracking-strategy 'switch-buffer)
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

(provide 'python)
