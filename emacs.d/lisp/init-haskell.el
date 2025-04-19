;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :straight (haskell-mode :type git
                          :host github
                          :repo "haskell/haskell-mode"))

(use-package haskell-ts-mode
  :disabled t
  :straight (:type git :host codeberg :repo "pranshu/haskell-ts-mode")
  :config
  (setq haskell-ts-highlight-signature t))


(provide 'init-haskell)
;;; init-haskell.el ends here
