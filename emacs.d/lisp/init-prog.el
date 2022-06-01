(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  :config
  (global-prettify-symbols-mode t))

(use-package protobuf-mode)

(use-package yaml-mode)

(use-package terraform-mode)

(use-package csv-mode)

(use-package cask-mode)

(use-package csharp-mode)

(use-package lua-mode)

(use-package vimrc-mode)

(use-package plantuml-mode
  :mode "\\.puml\\'"
  :custom
  (plantuml-jar-path "/usr/local/bin/plantuml")
  (plantuml-default-exec-mode 'executable))

(use-package graphviz-dot-mode)

(provide 'init-prog)
;;; init-prog.el ends here
