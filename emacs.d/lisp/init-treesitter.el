(use-package tree-sitter
  :disabled t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode)

  ;; Makes every node a link to a section of code
  (setq tree-sitter-debug-jump-buttons t)
  ;; highlights entire sub tree in your code
  (setq tree-sitter-debug-highlight-jump-region t)

  (global-set-key (kbd "C-c t D") 'tree-sitter-debug-mode))

(use-package tree-sitter-langs :disabled t :after tree-siter)

(use-package tree-sitter-indent :disabled t)

(use-package treesit-auto
  :config
  (setq treesit-font-lock-level 4)
  (global-treesit-auto-mode))

(use-package treesitter-context
  :after (treesit-auto posframe-plus)
  :straight (:type git :host github :repo "zbelial/treesitter-context.el" :files ("*.el"))
  :commands (treesitter-context-toggle-show)
  :config
  (setq treesitter-context-idle-time 0.5
        treesitter-context-show-context-always t
        treesitter-context-frame-autohide-timeout 15))

(use-package combobulate
  :straight (combobulate :type git :host github :repo "mickeynp/combobulate" :branch "development")
  :custom
  (combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (go-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)))

;; (use-package tree-sitter-langs)

;; (use-package tree-sitter-indent)


(defun khz/toggle-fold ()
  (interactive)
  (if (equal tree-sitter-mode nil)
      (call-interactively 'evil-toggle-fold)
    (call-interactively 'ts-fold-toggle)))

(use-package ts-fold
  :disabled t
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
  :disabled t
  :straight (ts-fold-indicators :type git :host github :repo "emacs-tree-sitter/ts-fold"))

(use-package evil-textobj-tree-sitter
  :straight (evil-textobj-tree-sitter :type git
                                      :host github
                                      :repo "meain/evil-textobj-tree-sitter"
                                      :files (:defaults "queries"))
  :config
  (defvar +tree-sitter-inner-text-objects-map (make-sparse-keymap))
  (defvar +tree-sitter-outer-text-objects-map (make-sparse-keymap))
  (defvar +tree-sitter-goto-previous-map (make-sparse-keymap))
  (defvar +tree-sitter-goto-next-map (make-sparse-keymap))

  (evil-define-key '(visual operator) 'tree-sitter-mode
    "i" +tree-sitter-inner-text-objects-map
    "a" +tree-sitter-outer-text-objects-map)
  (evil-define-key 'normal 'tree-sitter-mode
    "[g" +tree-sitter-goto-previous-map
    "]g" +tree-sitter-goto-next-map))

(provide 'init-treesitter)
