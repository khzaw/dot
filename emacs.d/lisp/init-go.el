(use-package go-mode
  :config (setq gofmt-command "goimports"))

(use-package go-playground
  :diminish
  :commands (go-playground-mode))

(use-package go-dlv)

;; (use-package go-fill-struct :defer t)

;; (use-package go-impl :defer t)

(use-package gotest
  :bind (:map go-mode-map
          ("C-c C-t f" . go-test-current-file)
          ("C-c C-t t" . go-test-current-test)
          ("C-c C-t p" . go-test-current-project)
          ("C-c C-t c" . go-test-current-coverage)
          ("C-c C-t b" . go-test-current-benchmark)
          ("C-c C-x" . go-run)))

(use-package gorepl-mode
  :if (executable-find "gore")
  :hook (go-mode . gorepl-mode))

(use-package flycheck-golangci-lint
  :after (flycheck go-mode)
  :hook (go-mode . flycheck-golangci-lint-setup)
  :config
  (setq flycheck-golangci-lint-fast t))

(provide 'init-go)
;;; init-go.el ends here
