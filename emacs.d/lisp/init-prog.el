(use-package prog-mode
  :straight (:type built-in)
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  :config
  (global-prettify-symbols-mode t))

(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
          ("C-`" . hs-toggle-hiding)))

(use-package protobuf-mode)

(use-package yaml-mode)

(use-package terraform-mode)

(use-package csv-mode)

(use-package cask-mode)

(use-package lua-mode)

(use-package vimrc-mode)

(use-package plantuml-mode
  :mode "\\.puml\\'"
  :custom
  (plantuml-jar-path "/usr/local/bin/plantuml")
  (plantuml-default-exec-mode 'executable))

(use-package graphviz-dot-mode
  :commands graphviz-dot-mode
  :mode ("\\.dot'" . graphviz-dot-mode))

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

(use-package ssh-config-mode)

(use-package vyper-mode)

(use-package conf-mode
  :ensure nil
  :mode (rx (or ".list"
              "CODEOWNERS"
              (and ".env" (* (and "." (+ word))))
              (and "." (+ word) "rc"))
          eos))

(provide 'init-prog)
;;; init-prog.el ends here
