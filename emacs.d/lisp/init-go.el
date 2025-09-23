;; -*- lexical-binding: t; -*-

(use-package go-mode
  :config (setq gofmt-command "gofumpt"))

(use-package go-ts-mode
  :preface
  (declare-function treesit-node-text "treesit")
  (declare-function go-ts-mode--defun-name "go-ts-mode")

  (defun my/go-ts-mode-init ()
    (setq-local go-test-args "-count=1") ; don't use cached results
    (setq-local treesit-simple-imenu-settings
                '(("Constant" "\\`const_spec\\'" nil nil)
                  ("Function" "\\`function_declaration\\'" nil nil)
                  ("Interface" "\\`type_declaration\\'" go-ts-mode--interface-node-p nil)
                  ("Method" "\\`method_declaration\\'" nil nil)
                  ("New Type" "\\`type_declaration\\'" go-ts-mode--other-type-node-p nil)
                  ("Struct" "\\`type_declaration\\'" go-ts-mode--struct-node-p nil)
                  ("Type Alias" "\\`type_declaration\\'" go-ts-mode--alias-node-p nil)
                  ;; Unfortunately, this also includes local variables.
                  ("Variable" "\\`var_spec\\'" nil nil)))
    ;; Install custom function for Go node names.
    (setq-local treesit-defun-name-function #'my/treesit-go-defun-name))

  (defun my/treesit-go-defun-name (node)
    "Return the defun name of NODE for Go node types."
    (pcase (treesit-node-type node)
      ((or "const_spec" "var_spec")
       (treesit-node-text (treesit-node-child-by-field-name node "name") t))
      (_ (go-ts-mode--defun-name node))))

  :hook (go-ts-mode . my/go-ts-mode-init)
  :bind (:map go-ts-mode-map
         ("C-c C-a" . go-import-add)
         ("C-c C-d" . godef-describe)
         ("C-c C-j" . godef-jump))
  :config
  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config
                 '(go-ts-mode
                   :types ((?c "Constant" my/imenu-constant-face)
                           (?f "Function" my/imenu-function-face)
                           (?i "Interface" my/imenu-trait-face)
                           (?m "Method" my/imenu-method-face)
                           (?t "New Type" my/imenu-type-face)
                           (?s "Struct" my/imenu-struct-face)
                           (?a "Type Alias" my/imenu-type-face)
                           (?v "Variable" my/imenu-variable-face))))))

(use-package go-playground
  :diminish
  :custom
  (go-playground-init-command "go mod init playground")
  :commands (go-playground-mode))

(use-package go-dlv)

(use-package gotest
  :after go-ts-mode
  :preface
  (defun my/go-test-verbose (fn)
    "Run the Go test function FN with the verbose flag enabled."
    (require 'gotest)
    (let ((go-test-verbose t))
      (funcall fn)))
  :bind (:map go-ts-mode-map
              ("C-c C-t f" . go-test-current-file)
              ("C-c C-t t" . go-test-current-test)
              ("C-c C-t p" . go-test-current-project)
              ("C-c C-t c" . go-test-current-coverage)
              ("C-c C-t b" . go-test-current-benchmark)
              ("C-c C-t T" . (lambda () (interactive) (my/go-test-verbose #'go-test-current-test)))
              ("C-c C-t F" . (lambda () (interactive) (my/go-test-verbose #'go-test-current-file)))
              ("C-c C-t P" . (lambda () (interactive) (my/go-test-verbose #'go-test-current-project)))
              ("C-c C-x" . go-run)))

(use-package go-tag
  :after go-ts-mode
  :if (executable-find "gomodifytags")
  :bind (:map go-ts-mode-map
         ("C-c t a" . go-tag-add)
         ("C-c t r" . go-tag-remove))
  :init (setq go-tag-args (list "-transform" "snakecase")))

(use-package godoctor)

(use-package go-fill-struct
  :if (executable-find "fillstruct"))

(use-package go-impl
  :if (executable-find "impl"))

(use-package go-gen-test
    :bind (:map go-mode-map
           ("C-c t g" . go-gen-test-dwim)))

(use-package gorepl-mode
  :if (executable-find "gore")
  :hook ((go-mode go-ts-mode) . gorepl-mode)
  :bind (:map gorepl-mode-map
         ("C-c C-g" . nil))) ; Unbind C-c C-g, it's very easy to hit, I will type gorepl-run manually

(use-package flycheck-golangci-lint
  :disabled t
  :if (executable-find "golangci-lint")
  :after (flycheck-mode go-mode)
  :hook ((go-mode . flycheck-golangci-lint-setup)
         (go-ts-mode . flycheck-golangci-lint-setup)))

(use-package flymake-golangci
  :disabled t
  :after (go-mode flymake-mode)
  :hook ((go-mode . flymake-golangci-load)
         (go-ts-mode . flymake-golangci-load)))

(use-package go-stacktracer :commands (go-stacktracer-region))

(provide 'init-go)
;;; init-go.el ends here
