;; -*- lexical-binding: t; -*-
(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package astro-ts-mode
  :straight (:type git :host github :repo "Sorixelle/astro-ts-mode")
  :mode "\\.astro\\'"
  :config
  (defclass eglot-astro (eglot-lsp-server) ()
    :documentation "Astro language server")

  (cl-defmethod eglot-initialization-options ((server eglot-astro))
    "Required initialization options for Astro language server."
    `(:typescript
      (:tsdk
       ,(expand-file-name (concat (project-root (project-current)) "node_modules/typescript/lib")))))
  (add-hook 'astro-ts-mode-hook 'eglot-ensure)
  (add-to-list 'eglot-server-programs '(astro-ts-mode . (eglot-astro "astro-ls" "--stdio"))))

(use-package css-mode
  :init (setq css-indent-offset 2))

(use-package json-ts-mode
  :mode ("\\.json\\'"))

(use-package add-node-modules-path
  :hook ((js-mode
          js2-mode
          rjsx-mode
          tsx-ts-mode
          typescript-ts-mode
          web-mode) . add-node-modules-path))

(use-package prettier-js
  :straight (:type git :host github :repo "prettier/prettier-emacs")
  :after (add-node-modules-path)
  :hook ((js-mode js2-mode rjsx-mode tsx-ts-mode typescript-ts-mode web-mode) . prettier-js-mode))

(use-package js2-mode
  :init (setq js-indent-level 2))
(add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . js2-mode))

(use-package rjsx-mode
  :disabled t
  :mode ("\\.jsx?\\'"))

(use-package php-mode)

(defun setup-tide-mode()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (prettier-mode)
  (company-mode +1))

(use-package tsx-ts-mode
  :straight (:type built-in)
  :mode "\\.tsx\\'")

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode "\\.ts\\'")

(use-package typescript-mode
  :disabled t
  :init
  (autoload 'typescript-tsx-mode "typescript-mode" nil t)
  (add-to-list 'auto-mode-alist
               (cons "\\.tsx\\'" #'typescript-tsx-mode))
  ;; (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  ;; (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
  ;; (flycheck-add-mode 'javascript-tslint 'typescript-tsx-mode)
  ;; (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint) 'append)
  ;; (flycheck-add-mode 'typescript-tide 'typescript-tsx-mode)
  :config
  (when (fboundp 'web-mode)
    (define-derived-mode typescript-tsx-mode web-mode "TypeScript-TSX")
    (add-to-list 'lsp--formatting-indent-alist '(typescript-tsx-mode . typescript-indent-level))
    (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist '(typescript-tsx-mode . "tsx"))
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))
  (add-hook 'typescript-tsx-mode-hook tree-sitter-hl-use-font-lock-keywords nil))


;; (use-package tsi
;;   :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
;;   :after tree-sitter
;;   ;; define autoload definitions which when actually invoked will cause package to be loaded
;;   :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
;;   :init
;;   (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
;;   (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
;;   (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1))))

;; (use-package tide
;;   :after (typescript-mode company flycheck)
;;   :hook (((typescript-mode js-mode js2-mode rjsx-mode) . setup-tide-mode)
;;           (before-save . tide-format-before-save)))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (html-mode . emmet-mode)))

(provide 'init-web)
;;; init-web.el ends here
