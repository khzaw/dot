;; -*- lexical-binding: t; -*-

(use-package rust-mode
  :custom
  (rust-mode-treesitter-derive t)
  :hook (rust-mode . eglot-ensure))

(use-package rustic
  :straight (:type git :host github :repo "emacs-rustic/rustic")
  :after (rust-mode)
  :custom
  (rustic-lsp-client 'eglot))

(use-package rust-playground)


(provide 'init-rust)
