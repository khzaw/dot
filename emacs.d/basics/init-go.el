;;; package --- init-go.el
;;; Commentary:
;;; Code:
(use-package go-mode
  :hook ((go-mode . lsp-deferred)
          (before-save . lsp-format-buffer)
          (before-save . lsp-organize-imports))
  :bind (:map go-mode-map
          ("C-c C-t t" . go-test-current-test)
          ("C-c C-t f" . go-test-current-file)
          ("C-c C-t p" . go-test-current-project))
  :config (setq gofmt-command "goimports"))

(use-package go-snippets :defer t)

(use-package go-playground
  :diminish
  :commands (go-playground-mode))

(provide 'init-go)
;;; init-go.el ends here
