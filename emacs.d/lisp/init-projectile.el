(use-package projectile
  :delight '(:eval (concat " " (projectile-projct-name)))
  :bind (:map projectile-mode-map
          ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-sort-order 'recentf
    projectile-use-git-grep t
    projectile-enable-caching t
    projectile-completion-system 'default))

;; (use-package project
;;   :pin gnu
;;   :bind (("C-c k" . #'project-kill-buffers)
;;           ("C-c m" . #'project-compile)
;;           ("C-x f" . #'find-file)
;;           ("C-c f" . #'project-find-file)
;;           ("C-c F" . #'project-switch-project))
;;   :custom
;;   ;; This is one of my favorite things: you can customize
;;   ;; the options shown upon switching projects.
;;   (project-switch-commands
;;     '((project-find-file "Find file")
;;        (magit-project-status "Magit" ?g)
;;        (deadgrep "Grep" ?h)))
;;   (compilation-always-kill t)
;;   (project-vc-merge-submodules nil)
;;   )

(provide 'init-projectile)
;;; init-projectile.el ends here
