;; (use-package treesit
;;   :disabled t
;;   :if (executable-find "tree-sitter")
;;   :config
;;   (setq treesit-extra-load-path '("~/Code/dot/emacs.d/treesit-libs")))

(use-package tree-sitter-langs)

(use-package tree-sitter
  :after (tree-sitter-langs)
  :config (global-tree-sitter-mode)
  (setq tree-sitter-debug-jump-buttons t)
  (setq tree-sitter-debug-highlight-jump-region nil)
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
  :after tree-sitter
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :config (global-ts-fold-mode)
  (defun khz/toggle-fold ()
    (interactive)
    (if (equal tree-sitter-mode nil)
      (call-interactively 'evil-toggle-fold)
      (call-interactively 'ts-fold-toggle)))
  :hook (tree-sitter-after-on . (lambda ()
                                  (tree-sitter-hl-mode)
                                  (origami-mode -1)
                                  (ts-fold-mode 1)))
  :custom-face
  (tree-sitter-hl-face:property ((t (:slant normal))))
  (tree-sitter-hl-face:function.call ((t (:weight normal))))
  (tree-sitter-hl-face:method.call ((t (:weight normal))))
  ;; TODO <TAB> in normal mode should fold
  :bind ("C-`" . ts-fold-toggle))

(use-package ts-fold-indicators
  :straight (ts-fold-indicators :type git :host github :repo "emacs-tree-sitter/ts-fold"))

(use-package evil-textobj-tree-sitter
  :straight (evil-textobj-tree-sitter :type git
              :host github
              :repo "meain/evil-textobj-tree-sitter"
              :files (:defaults "queries")))

(provide 'init-treesitter)
