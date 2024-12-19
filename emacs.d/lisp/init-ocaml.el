;; -*- lexical-binding: t; -*-

(use-package tuareg
  :config
  (setq tuareg-indent-align-with-first-arg t)
  (setq tuareg-match-patterns-aligned t)
  (setq tuareg-opam-insinuate t)
  :hook ((tuareg-mode . (lambda ()
                          (define-key evil-insert-state-local-map (kbd "RET")
                            'electric-newline-and-maybe-indent)
                          (setq prettify-symbols-alist
                                (append tuareg-prettify-symbols-basic-alist
                                        tuareg-prettify-symbols-extra-alist
                                        prettify-symbols-alist))
                          (prettify-symbols-mode 1))))
  :custom
  (tuareg-font-lock-governing-face ((t :inherit font-lock-keyword-face))))

(use-package dune
  :straight (:type git :host github :repo "ocaml/dune" :depth 1 :files ("editor-integration/emacs/*.el"))
  :when (executable-find "dune"))

(use-package merlin
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode))

(use-package merlin-eldoc
  :hook (tuareg-mode . merlin-eldoc-setup))


(use-package ocp-indent
  :when (executable-find "ocp-indent")
  :commands (ocp-indent-region ocp-indent-buffer ocp-indent-line)
  :hook ((tuareg-mode . ocp-setup-indent)
         (tuareg-mode . (lambda ()
                          (setq-local indent-line-function #'ocp-indent-line)
                          (setq-local indent-region-function #'ocp-indent-region)))))

(use-package utop
  :when (executable-find "opam")
  :config
  (setq utop-command "opam exec -- utop -emacs")
  :hook ((turaeg-mode) . utop-minor-mode))

(use-package ocamlformat
  :after tuareg
  :when (executable-find "ocamlformat")
  :custom
  (ocamlformat-enable 'enable-outside-detected-project)
  :hook (tuareg-mode . (lambda ()
                         (add-hook 'before-save-hook #'ocamlformat-before-save nil t))))



(provide 'init-ocaml)
;; init-ocaml.el ends here
