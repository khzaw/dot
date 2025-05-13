;; -*- lexical-binding: t; -*-

(defun set-kubeconfig (config-path)
  "Set the KUBECONFIG environment variable to the specified path."
  (interactive "fSelect kubeconfig file: ")
  (setenv "KUBECONFIG" config-path)
  (message "KUBECONFIG set to %s" config-path))

(defun set-homelab-kubeconfig ()
  "Set KUBECONFIG to homelab cluster"
  (interactive)
  (set-kubeconfig "~/Code/rangoonpulse/kubeconfig"))

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
