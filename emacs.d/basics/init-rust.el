;;; package --- init-prog.el
;;; Commentary:

;;; Code:
(use-package rustic
  :hook (rust-mode . lsp-deferred)
  :bind (:map rustic-mode-map
          ("M-j" . lsp-ui-imenu)
          ("M-?" . lsp-find-references)
          ("C-c C-c l" . flycheck-list-errors)
          ("C-c C-c a" . lsp-execute-code-action)
          ("C-C C-c r" . lsp-rename)
          ("C-C C-c q" . lsp-workspace-restart)
          ("C-C C-c Q" . lsp-workspace-shutdown)
          ("C-C C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :diminish cargo-minor-mode)

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rust-playground)

(provide 'init-rust)
;;; init-rust.el ends here
