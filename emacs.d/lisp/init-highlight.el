
;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook ((after-init . global-hl-line-mode)
          ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
            (lambda () (setq-local global-hl-line-mode nil)))))

;; Highlight matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
          show-paren-when-point-in-periphery t))

(provide 'init-highlight)
;;; init-highlight.el ends here
