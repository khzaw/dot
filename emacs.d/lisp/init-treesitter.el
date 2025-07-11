;; -*- lexical-binding: t; -*-

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
   (go-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)))


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
  :hook ((prog-mode . treesit-fold-mode)
         ;; (prog-mode . treesit-fold-indicators-mode)
         (prog-mode . treesit-fold-line-comment-mode))
  :config
  ;; (setq treesit-fold-indicators-fringe 'left-fringe)
  (setq treesit-fold-line-count-show 1))

(use-package symbols-outline
  :bind ("C-c e C-s" . symbols-outline-show)
  :custom
  (symbols-outline-window-width 55)
  (symbols-outline-ignore-variable-symbols nil)
  :config

  (symbols-outline-follow-mode 1)

  (defun khz/symbols-outline-fetch-fn ()
    (if (or (bound-and-true-p eglot--managed-mode)
            (bound-and-true-p lsp-managed-mode))
        #'symbols-outline-lsp-fetch
      ;; brew install universal-ctags
      #'symbols-outline-ctags-fetch))

  (advice-add 'symbols-outline-show :before
              (lambda (&rest _)
                (setq-local symbols-outline-fetch-fn (khz/symbols-outline-fetch-fn)))))

(use-package treesitter-context
  :straight (:type git :host github :repo "zbelial/treesitter-context.el" :files ("*.el"))
  :after (posframe-plus)
  :commands (treesitter-context-toggle-show)
  :bind (("C-c e C-t" . treesitter-context-toggle-show)
         ("C-c e F" . treesitter-context-focus-mode)
         (:map treesitter-context-fold-mode-map
               ("C-`" . treesitter-context-fold-toggle)))
  :config
  (setq treesitter-context-idle-time 0.5
        treesitter-context-show-context-always t
        treesitter-context-frame-autohide-timeout 15)
  ;; (treesitter-context-focus-mode 1)
)

(defun treesit-enabled-p ()
  "Checks if the current buffer has treesit parser."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-at (point))))

;; Show scope info of block
(use-package scopeline
  :commands (scopeline-mode)
  ;; :hook (prog-mode . (lambda () (when (treesit-enabled-p)
  ;;                                 (scopeline-mode))))
  :config (setq scopeline-overlay-prefix " ~"
                scopeline-min-lines 20))

;; provides grammatical edit based on treesit
(use-package fingertip
  :straight (:type git :host github :repo "manateelazycat/fingertip")
  :hook ((go-ts-mode            . fingertip-mode)
         (python-ts-mode        . fingertip-mode)
         (web-mode              . fingertip-mode)
         (js-ts-mode            . fingertip-mode)
         (typescript-ts-mode    . fingertip-mode)
         (makefile-gmake-mode   . fingertip-mode)
         (haskell-mode          . fingertip-mode)
         (lisp-mode             . fingertip-mode)
         (lisp-interaction-mode . fingertip-mode)
         (java-ts-mode          . fingertip-mode)
         (php-mode              . fingertip-mode)
         (css-ts-mode           . fingertip-mode)
         (rust-ts-mode          . fingertip-mode)
         (bash-ts-mode          . fingertip-mode)
         (emacs-lisp-mode       . fingertip-mode))
  :bind (:map fingertip-mode-map
              ("("   . fingertip-open-round)
              ("["   . fingertip-open-bracket)
              ("{"   . fingertip-open-curly)
              (")"   . fingertip-close-round)
              ("]"   . fingertip-close-bracket)
              ("}"   . fingertip-close-curly)
              ("="   . fingertip-equal)
              ("\""  . fingertip-double-quote)
              ("'"   . fingertip-single-quote)
              ("SPC" . fingertip-space)
              ("RET" . fingertip-newline)
              ;; ("M-o" . fingertip-backward-delete)
              ("C-k" . fingertip-kill)))



(provide 'init-treesitter)
