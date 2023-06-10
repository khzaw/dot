(use-package go-mode
  :config (setq gofmt-command "goimports"))

(use-package go-playground
  :diminish
  :commands (go-playground-mode))

(use-package go-dlv)

(use-package gotest
  :bind (:map go-mode-map
         ("C-c C-t f" . go-test-current-file)
         ("C-c C-t t" . go-test-current-test)
         ("C-c C-t p" . go-test-current-project)
         ("C-c C-t c" . go-test-current-coverage)
         ("C-c C-t b" . go-test-current-benchmark)
         ("C-c C-x" . go-run)))

(use-package go-tag
  :bind (:map go-mode-map
          ("C-c t a" . go-tag-add)
          ("C-c t r" . go-tag-remove))
  :init (setq go-tag-args (list "-transform" "camelcase")))

(use-package go-fill-struct)

(use-package go-dlv)

(use-package gorepl-mode
  :if (executable-find "gore")
  :hook (go-mode . gorepl-mode))

(use-package flycheck-golangci-lint
  :after (flycheck go-mode)
  :hook (go-mode . flycheck-golangci-lint-setup)
  :config
  (setq flycheck-golangci-lint-fast t))

(use-package flymake-golangci
  :after (go-mode flymake)
  :straight (:type git :host gitlab :repo "shackra/flymake-golangci")
  :hook (go-mode . flymake-golangci-load))

(provide 'init-go)
;;; init-go.el ends here
