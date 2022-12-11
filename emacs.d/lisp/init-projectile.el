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

(provide 'init-projectile)
;;; init-projectile.el ends here
