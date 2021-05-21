(use-package restclient)

(use-package web-mode
  :mode ("\\.html?\\'"
          "\\.tsx\\'"
          "\\.ejs?\\'")
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-auto-close-style 2
        web-mode-enable-current-element-highlight t
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t)
  (add-hook 'web-mode-hook
    (lambda ()
      (when (string-equal "tsx" (file-name-extension buffer-file-name))
        (setup-tide-mode)))))

(use-package css-mode)

(use-package add-node-modules-path
  :hook ((js-mode js2-mode rjsx-mode web-mode typescript-mode) . add-node-modules-path))

(use-package prettier-js
  :after (add-node-modeuls-path rjsx-mode web-mode typescript-mode)
  :hook (((rjsx-mode js-mode js2-mode web-mode typescript-mode) . add-node-modules-path)
          ((rjsx-mode js-mode js2-mode web-mode typescript-mode) . prettier-js-mode)))

(use-package rjsx-mode
  :defer t
  :mode ("\\.jsx?\\'"))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (prettier-js-mode)
  (company-mode +1))

(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . setup-tide-mode)
          (js-mode . setup-tide-mode)
          (js2-mode . setup-tide-mode)
          (rjsx-mode . setup-tide-mode)
          (before-save . tide-format-before-save))
  )

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
          (html-mode . emmet-mode)))


(provide 'web)
