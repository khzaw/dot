;; -*- lexical-binding: t; -*-

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :custom
  (projectile-sort-order 'recentf)
  (projectile-use-git-grep t)
  (projectile-enable-caching t)
  (projectile-verbose nil)
  (projectile-completion-system 'default)
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  :bind (:map projectile-mode-map
         ("C-c p B" . consult-projectile)
         ("C-c p f" . consult-projectile-find-file)))


;; (use-package project
;;   :pin gnu
;;   :bind (("C-c k" . #'project-kill-buffers)
;;           ("C-c m" . #'project-compile)
;;           ("C-x f" . #'find-file)
;;           ("C-c f" . #'project-find-file)
;;           ("C-c F" . #'project-switch-project))
;;   :custom
;;   (project-switch-commands
;;     '((project-find-file "Find file")
;;        (magit-project-status "Magit" ?g)
;;        (deadgrep "Grep" ?h)))
;;   (compilation-always-kill t)
;;   (project-vc-merge-submodules nil)
;;   )

(provide 'init-projectile)
;;; init-projectile.el ends here
