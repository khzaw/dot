;; -*- lexical-binding: t; -*-

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

(defun treesit--ts-find-parent (node pattern)
  (let ((pred (lambda (n)
                (string-match-p patterts-node-textts-node-textn (treesit-node-type n)))))
    (treesit-parent-until node pred)))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (setq treesit-font-lock-level 4)
  (let ((astro-recipe (make-treesit-auto-recipe
                       :lang 'astro
                       :ts-mode 'astro-ts-mode
                       :url "https://github.com/virchau13/tree-sitter-astro"
                       :revision "master"
                       :source-dir "src")))
    (add-to-list 'treesit-auto-recipe-list astro-recipe))
  (global-treesit-auto-mode))

(use-package treesitter-context
  :straight (:type git :host github :repo "zbelial/treesitter-context.el" :files ("*.el"))
  ;; :hook ((go-ts-mode . treesitter-context-focus-mode)
  ;;        (typescript-ts-mode . treesitter-context-focus-mode)
  ;;        (python-ts-mode . treesitter-context-focus-mode)
  ;;        (yaml-ts-mode . treesitter-context-focus-mode))
  :bind (("C-c e C-t" . treesitter-context-toggle-show)
         ("C-c e C-f" . treesitter-context-fold-mode)
         ("C-c e F" . treesitter-context-focus-mode)
         (:map treesitter-context-fold-mode-map
               ("C-`" . treesitter-context-fold-toggle)))
  :config
  (setq treesitter-context-idle-time 0.5
        treesitter-context-show-context-always t
        treesitter-context-frame-autohide-timeout 15)
  ;; (treesitter-context-focus-mode 1)
  (treesitter-context-fold-mode 1))

(use-package combobulate
  :straight (combobulate :type git :host github :repo "mickeynp/combobulate" :branch "development")
  :custom
  (combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (yaml-mode . combobulate-mode)
   ;; (go-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
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
  (tree-sitter-hl-face:attribute ((t (:weight normal)))))
;; TODO <TAB> in normal mode should fold

(use-package ts-fold-indicators
  :disabled t
  :straight (ts-fold-indicators :type git :host github :repo "emacs-tree-sitter/ts-fold"))

(use-package evil-textobj-tree-sitter
  :straight (evil-textobj-tree-sitter :type git
                                      :host github
                                      :repo "meain/evil-textobj-tree-sitter"
                                      :files (:defaults "queries" "treesit-queries"))
  :config
  ;; (defvar +tree-sitter-inner-text-objects-map (make-sparse-keymap))
  ;; (defvar +tree-sitter-outer-text-objects-map (make-sparse-keymap))
  ;; (defvar +tree-sitter-goto-previous-map (make-sparse-keymap))
  ;; (defvar +tree-sitter-goto-next-map (make-sparse-keymap))

  ;; (evil-define-key '(visual operator) 'tree-sitter-mode
  ;;   "i" +tree-sitter-inner-text-objects-map
  ;;   "a" +tree-sitter-outer-text-objects-map)
  ;; (evil-define-key 'normal 'tree-sitter-mode
  ;;   "[g" +tree-sitter-goto-previous-map
  ;;   "]g" +tree-sitter-goto-next-map))

  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;; You can also bind multiple items and we will match the first one we can find
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

  ;; The first arguemnt to `evil-textobj-tree-sitter-get-textobj' will be the capture group to use
  ;; and the second arg will be an alist mapping major-mode to the corresponding query to use.
  (define-key evil-outer-text-objects-map "m" (evil-textobj-tree-sitter-get-textobj "import"
                                                '((python-mode . [(import_statement) @import])
                                                  (rust-mode . [(use_declaration) @import]))))
  ;; Goto start of next function
  (define-key evil-normal-state-map
    (kbd "]f")
    (lambda ()
      (interactive)
      (evil-textobj-tree-sitter-goto-textobj "function.outer")))

  ;; Goto start of previous function
  (define-key evil-normal-state-map
    (kbd "[f")
    (lambda ()
      (interactive)
      (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))

  ;; Goto end of next function
  (define-key evil-normal-state-map
    (kbd "]F")
    (lambda ()
      (interactive)
      (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))

  ;; Goto end of previous function
  (define-key evil-normal-state-map
    (kbd "[F")
    (lambda ()
      (interactive)
      (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

(use-package treesit-jump
  :straight (:host github :repo "dmille56/treesit-jump" :files ("*.el" "treesit-queries"))
  :config
  ;; Optional: add some queries to filter out of results (since they can be too cluttered sometimes)
  (setq treesit-jump-queries-filter-list '("inner" "test" "param")))

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :config
  (setq treesit-fold-line-count-show 1)
  (global-treesit-fold-mode 1)
  (global-treesit-fold-indicators-mode 1)
  (add-hook 'treesit-fold-mode-hook
          (lambda ()
            (when (and (bound-and-true-p yafolding-mode)
                       (fboundp 'yafolding-mode))
              (yafolding-mode -1))))
  :bind (:map treesit-fold-mode-map
              ("C-<return>" . treesit-fold-toggle)))

(use-package symbols-outline
  :bind ("C-c e i" . symbols-outline-show)
  :custom
  (symbols-outline-fetch-fn #'symbols-outline-lsp-fetch)
  :config
  (symbols-outline-follow-mode 1))


(provide 'init-treesitter)
