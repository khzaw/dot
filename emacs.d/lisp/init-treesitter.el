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
  (defvar khz/tree-sitter-mappings '(("a" "parameter" ("parameter.inner") ("parameter.outer"))
                                     ("v" "conditional" ("conditional.inner" "loop.inner") ("conditional.outer" "loop.outer"))
                                     ("c" "class" ("class.inner") ("class.outer"))
                                     ("f" "function" ("function.inner") ("function.outer"))
                                     ("n" "comment" ("comment.outer") ("comment.outer"))))

  (dolist (mapping khz/tree-sitter-mappings)
    (let ((key (car mapping))
          (name (cadr mapping))
          (inner (caddr mapping))
          (outer (cadddr mapping)))
      ;; Need this weird `eval' here as `evil-textobj-tree-sitter-get-textobj' is a macro
      (eval `(define-key evil-inner-text-objects-map ,key (cons ,(concat "evil-inner-" name) (evil-textobj-tree-sitter-get-textobj ,inner))))
      (eval `(define-key evil-outer-text-objects-map ,key (cons ,(concat "evil-outer-" name) (evil-textobj-tree-sitter-get-textobj ,outer))))
      (define-key evil-normal-state-map (kbd (concat "]" key)) (cons (concat "goto-" name "-start") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj outer))))
      (define-key evil-normal-state-map (kbd (concat "[" key)) (cons (concat "goto-" name "-start") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj outer t))))
      (define-key evil-normal-state-map (kbd (concat "]" (upcase key))) (cons (concat "goto-" name "-end") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj outer nil t))))
      (define-key evil-normal-state-map (kbd (concat "[" (upcase key))) (cons (concat "goto-" name "-end") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj outer t t))))))

  (define-key evil-outer-text-objects-map "m"
              (evil-textobj-tree-sitter-get-textobj
                "import"
                '((python-mode . ((import_statement) @import))
                  (python-ts-mode . ((import_statement) @import))
                  (go-mode . ((import_spec) @import))
                  (go-ts-mode . ((import_spec) @import))
                  (rust-mode . ((use_declaration) @import))))))


(use-package treesit-jump
  :straight (:host github :repo "dmille56/treesit-jump" :files ("*.el" "treesit-queries"))
  :config
  ;; Optional: add some queries to filter out of results (since they can be too cluttered sometimes)
  (setq treesit-jump-queries-filter-list '("inner" "test" "param")))

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :bind (:map treesit-fold-mode-map
              ("C-<return>" . treesit-fold-toggle))
  :hook ((after-init . global-treesit-fold-mode)
         (after-init . global-treesit-fold-indicators-mode)
         (after-init . treesit-fold-line-comment-mode))
  :config
  (setq treesit-fold-indicators-fringe 'left-fringe)
  (setq treesit-fold-line-count-show 1))

(use-package symbols-outline
  :bind ("C-c C-s" . symbols-outline-show)
  :custom
  (symbols-outline-window-width 55)
  (symbols-outline-ignore-variable-symbols nil)
  :config
  ;; brew install universal-ctags
  (unless (executable-find "ctags")
    (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch))
  (symbols-outline-follow-mode 1))


(provide 'init-treesitter)
