;; (use-package treesit
;;   :disabled t
;;   :if (executable-find "tree-sitter")
;;   :config
;;   (setq treesit-extra-load-path '("~/Code/dot/emacs.d/treesit-libs")))

(use-package tree-sitter-langs)

(use-package tree-sitter
  :after (tree-sitter-langs)
  :config (global-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

;; (use-package tree-sitter
;;   :hook ((go-mode
;;           python-mode
;;           rust-mode
;;           ruby-mode
;;           js-mode js2-mode rjsx-mode typescript-mode typescript-ts-mode
;;           sh-mode) . tree-sitter-hl-mode)
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs :after tree-sitter :defer nil
;;   :config
;;   (tree-sitter-require 'tsx)
;;   (add-to-list 'tree-sitter-major-mode-language-alist
;;     '(typescript-tsx-mode . tsx)))

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))

(use-package ts-fold-indicators
  :straight (ts-fold-indicators :type git :host github :repo "emacs-tree-sitter/ts-fold"))

(provide 'init-treesitter)
