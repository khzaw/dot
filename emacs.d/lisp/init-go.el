;; -*- lexical-binding: t; -*-

(use-package go-mode
  :config (setq gofmt-command "gofumpt"))

(use-package go-ts-mode
  :bind (:map go-ts-mode-map
         ("C-c C-a" . go-import-add)
         ("C-c C-d" . godef-describe)
         ("C-c C-j" . godef-jump)))

(use-package go-playground
  :diminish
  :commands (go-playground-mode))

(use-package go-dlv)

(use-package gotest
  :after go-ts-mode
  :bind (:map go-ts-mode-map
         ("C-c C-t f" . go-test-current-file)
         ("C-c C-t t" . go-test-current-test)
         ("C-c C-t p" . go-test-current-project)
         ("C-c C-t c" . go-test-current-coverage)
         ("C-c C-t b" . go-test-current-benchmark)
         ("C-c C-x" . go-run)))

(use-package go-tag
  :after go-ts-mode
  :if (executable-find "gomodifytags")
  :bind (:map go-ts-mode-map
         ("C-c t a" . go-tag-add)
         ("C-c t r" . go-tag-remove))
  :init (setq go-tag-args (list "-transform" "camelcase")))

(use-package godoctor)

(use-package go-fill-struct)

(use-package gorepl-mode
  :if (executable-find "gore")
  :hook ((go-mode go-ts-mode) . gorepl-mode)
  :bind (:map gorepl-mode-map
         ("C-c C-g" . nil))) ; Unbind C-c C-g, it's very easy to hit, I will type gorepl-run manually

(use-package flycheck-golangci-lint
  :after (flycheck go-mode)
  :hook ((go-mode . flycheck-golangci-lint-setup)
         (go-ts-mode . flycheck-golangci-lint-setup))
  :config
  (setq flycheck-golangci-lint-fast t))

(use-package flymake-golangci
  :disabled t
  :after (go-mode flymake)
  :hook ((go-mode . flymake-golangci-load)
         (go-ts-mode . flymake-golangci-load)))

(provide 'init-go)
;;; init-go.el ends here
