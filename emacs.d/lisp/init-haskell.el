(use-package haskell-mode :defer t)

(use-package haskell-ts-mode
  :disabled t
  :straight (:type git :host codeberg :repo "pranshu/haskell-ts-mode")
  :config
  (setq haskell-ts-highlight-signature t))


(provide 'init-haskell)
;;; init-haskell.el ends here
