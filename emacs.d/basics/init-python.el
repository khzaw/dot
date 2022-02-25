;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package python
  :hook (python-mode . lsp-deferred)
  :config
    ;; Set iPython as default interpreter.
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"))
    (setq python-shell-interpreter-args "")
    (setq python-indent-guess-indent-offset-verbose nil))

(use-package pyvenv
  :after python
  :config (pyvenv-mode t))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))
(use-package poetry
  :after python
  :init
  (setq poetry-tracking-strategy 'switch-buffer)
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

(use-package jupyter)

(provide 'init-python)
;;; init-python.el ends here
