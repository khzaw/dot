(use-package prog-mode
  :ensure nil
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

(use-package protobuf-mode :defer t)

(use-package yaml-mode :defer t)

(use-package terraform-mode :defer t)

(use-package csv-mode :defer t)

(use-package cask-mode :defer t)

(use-package csharp-mode :defer t)

(use-package lua-mode :defer t)

(use-package vimrc-mode :defer t)

(use-package plantuml-mode
  :defer t
  :mode "\\.puml\\'"
  :custom
  (plantuml-jar-path "/usr/local/bin/plantuml")
  (plantuml-default-exec-mode 'executable))

(use-package graphviz-dot-mode
  :defer t
  :commands graphviz-dot-mode
  :mode ("\\.dot'" . graphviz-dot-mode))

(use-package tree-sitter
  :hook ((go-mode python-mode rust-mode ruby-mode js-mode js2-mode rjsx-mode c-mode typescript-mode sh-mode) . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs :after tree-sitter :defer nil
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist
    '(typescript-tsx-mode . tsx)))

(use-package ssh-config-mode :defer t)

(use-package vyper-mode :defer t)

(use-package conf-mode
  :ensure nil
  :mode (rx (or ".list"
              "CODEOWNERS"
              (and ".env" (* (and "." (+ word))))
              (and "." (+ word) "rc"))
          eos))

(provide 'init-prog)
;;; init-prog.el ends here
