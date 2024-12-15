;; -*- lexical-binding: t; -*-

(use-package tuareg
  :config
  (setq tuareg-opam-insinuate t))

(use-package dune
  :straight (:type git :host github :repo "ocaml/dune" :depth 1 :files ("editor-integration/emacs/*.el")))

(use-package merlin
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode))

(use-package merlin-eldoc
  :hook ((tuareg-mode) . merlin-eldoc-setup))

(use-package ocp-indent)

(use-package utop
  :config
  (setq utop-command "opam exec -- utop -emacs")
  :hook ((turage-mode) . utop-minor-mode))


(provide 'init-ocaml)
;; init-ocaml.el ends here
