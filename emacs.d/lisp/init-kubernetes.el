;; -*- lexical-binding: t; -*-

(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600)
  (fset 'k8s 'kubernetes-overview))

(use-package kubernetes-evil
  :after kubernetes)

(use-package kubel
  :after (vterm)
  :config (kubel-vterm-setup))

(use-package kubernetes-helm
  :straight (:type git :host github :repo "abrochard/kubernetes-helm"))

(provide 'init-kubernetes)
