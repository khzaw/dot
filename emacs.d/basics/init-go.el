;;; package --- init-go.el

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
          (before-save . lsp-format-buffer)
          (before-save . lsp-organize-imports))
  :bind (:map go-mode-map
          ("C-c a t" . #'go-test-current-test)))

(use-package go-snippets :defer t)

(use-package go-playground
  :diminish
  :commands (go-playground-mode))

(use-package company-go
  :after (company)
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))

(provide 'init-go)
