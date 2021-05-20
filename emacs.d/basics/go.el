;;; package --- go.el

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
          (before-save . lsp-format-buffer)
          (before-save . lsp-organize-imports)))

(use-package go-playground
  :diminish
  :commands (go-playground-mode))

(provide 'go)
