(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

(use-package json-mode)

;; (use-package add-node-modules-path
;;   :hook ((js-mode
;;            js2-mode
;;            rjsx-mode
;;            web-mode
;;            typescript-mode
;;            typescript-tsx-mode) . add-node-modules-path))

;; (use-package prettier
;;   :after (add-node-modules-path)
;;   :hook ((js-mode
;;            js2-mode
;;            rjsx-mode
;;            web-mode
;;            typescript-mode
;;            typescript-tsx-mode
;;            solidity-mode) . prettier-mode))

;; (use-package js2-mode
;;   :init (setq js-indent-level 2))

;; (use-package rjsx-mode
;;   :mode ("\\.jsx?\\'"))

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

(use-package typescript-mode
  :after tree-sitter
  :config
  (define-derived-mode typescript-tsx-mode typescript-mode "Typescript TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package tsi
  :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
  :after tree-sitter
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1))))

;; (use-package tide
;;   :after (typescript-mode company flycheck)
;;   :hook (((typescript-mode js-mode js2-mode rjsx-mode) . setup-tide-mode)
;;           (before-save . tide-format-before-save)))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
          (html-mode . emmet-mode)))

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)
          ("\\.restclient$" . restclient-mode))
  :config
  (use-package restclient-test
    :diminish
    :hook (restclient-mode . restclient-test-mode))

  (with-eval-after-load 'company
    (use-package company-restclient
      :init (add-to-list 'company-backends 'company-restclient))))

(provide 'init-web)
;;; init-web.el ends here
