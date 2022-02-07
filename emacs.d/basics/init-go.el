;;; package --- init-go.el
;;; Commentary:
;;; Code:
(use-package go-mode
  :hook ((go-mode . lsp-deferred)
          (before-save . lsp-format-buffer)
          (before-save . lsp-organize-imports))
  :bind (:map go-mode-map
          ("C-x a t" . #'go-test-current-test)))

(use-package go-snippets :defer t)

(use-package go-playground
  :diminish
  :commands (go-playground-mode))

(provide 'init-go)
;;; init-go.el ends here
