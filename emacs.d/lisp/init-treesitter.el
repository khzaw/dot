(use-package tree-sitter-langs)

(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode)
  ;; Makes every node a link to a section of code
  (setq tree-sitter-debug-jump-buttons t)
  ;; highlights entire sub tree in your code
  (setq tree-sitter-debug-highlight-jump-region t)
  (use-package combobulate
    :straight (combobulate :type git :host github :repo "mickeynp/combobulate")
    :preface
    (setq combobulate-key-prefix "C-c o")
    :hook
    ((python-ts-mode . combobulate-mode)
     (js-ts-mode . combobulate-mode)
     (css-ts-mode . combobulate-mode)
     (yaml-ts-mode . combobulate-mode)
     (typescript-tsx-mode . combobulate-mode)
     (tsx-ts-mode . combobulate-mode))))

(defun khz/toggle-fold ()
  (interactive)
  (if (equal tree-sitter-mode nil)
      (call-interactively 'evil-toggle-fold)
    (call-interactively 'ts-fold-toggle)))

(use-package ts-fold
  :after tree-sitter
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :config (global-ts-fold-mode)
  :custom-face
  (tree-sitter-hl-face:property ((t (:slant normal))))
  (tree-sitter-hl-face:function.call ((t (:weight normal))))
  (tree-sitter-hl-face:method.call ((t (:weight normal))))
  (tree-sitter-hl-face:attribute ((t (:weight normal))))
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
