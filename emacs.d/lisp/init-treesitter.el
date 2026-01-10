;; -*- lexical-binding: t; -*-

(use-package treesit
  :straight (:type built-in)
  :init (setq treesit-font-lock-level 4)
  :config
    ;; --- PERFORMANCE FIX ---
  ;; This cache ensures that checking if a grammar is available is near-instant.
  ;; This neutralizes the lag caused by treesit-auto checking 80+ languages repeatedly.
  (defvar khz/treesit-lang-cache
    (make-hash-table :test 'equal)
    "Cache the expensive computation of treesit language availability.")

  (defun khz/treesit-language-available-p (fn lang &rest rest)
    "Caching around the CPU expensive `treesit-language-available-p'."
    (let ((cached-value (gethash lang khz/treesit-lang-cache 'miss)))
      (if (eq 'miss cached-value)
          (let ((value (apply fn lang rest)))
            (puthash lang value khz/treesit-lang-cache)
            value)
        cached-value)))

  (advice-add #'treesit-language-available-p :around #'khz/treesit-language-available-p)

  (defun khz/treesit-goto-function-name ()
    "Move point to the function name of the current function definition."
    (interactive)
    (when-let* ((node (treesit-node-at (point)))
                ;; Define what nodes look like a "function" in various languages
                (func-node-types '("function_definition" ; Go, JS/TS
                                   "method_declaration"  ; Go, Java
                                   "func_literal"        ; Go (anonymous)
                                   "function_definition" ; Python
                                   "method_definition"   ; JS/TS classes
                                   "function_item"))
                ;; Walk up the tree until we find one of those nodes
                (parent (treesit-parent-until
                         node
                         (lambda (n)
                           (member (treesit-node-type n) func-node-types)))))
      (if-let ((name-node (treesit-node-child-by-field-name parent "name")))
          (goto-char (treesit-node-start name-node))
        ;; Fallback
        (goto-char (treesit-node-start parent)))))

  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "g F") #'khz/treesit-goto-function-name)))

(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  ;; Astro recipe
  (let ((astro-recipe (make-treesit-auto-recipe
                       :lang 'astro
                       :ts-mode 'astro-ts-mode
                       :url "https://github.com/virchau13/tree-sitter-astro"
                       :revision "master"
                       :source-dir "src"
                       :ext "\\.astro\\'")))
    (add-to-list 'treesit-auto-recipe-list astro-recipe)
    (add-to-list 'treesit-auto-langs 'astro))

  ;; D2 recipe
  (let ((d2-recipe (make-treesit-auto-recipe
                    :lang 'd2
                    :ts-mode 'd2-ts-mode
                    :remap 'd2-mode
                    :url "https://github.com/ravsii/tree-sitter-d2"
                    :revision "main"
                    :source-dir "src"
                    :ext "\\.d2\\'")))
    (add-to-list 'treesit-auto-recipe-list d2-recipe)
    (add-to-list 'treesit-auto-langs 'd2))

  (treesit-auto-add-to-auto-mode-alist 'all)
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
  :disabled t
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
              ;; ("\""  . fingertip-double-quote)
              ;; ("'"   . fingertip-single-quote)
              ("SPC" . fingertip-space)
              ("RET" . fingertip-newline)
              ;; ("M-o" . fingertip-backward-delete)
              ("C-k" . fingertip-kill)))


(provide 'init-treesitter)
