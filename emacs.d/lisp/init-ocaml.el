
(use-package tuareg
  :config
  (setq tuareg-opam-insinuate t))

(use-package dune
  :disabled t
  :straight (:type git :host github :repo "ocaml/dune" :files ("editor-integrations/emacs/*.el")))

(use-package merlin
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode))

(use-package merlin-eldoc
  :hook ((tuareg-mode) . merlin-eldoc-setup))

(use-package ocp-indent)


(provide 'init-ocaml)
;; init-ocaml.el ends here
