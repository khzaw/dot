(use-package treemacs
 :commands (treemacs-follow-mode
              treemacs-filewatch-mode
              treemacs-fringe-indicator-mode
              treemacs-git-mode)
  :config
  (progn
    (setq
      treemacs-eldoc-display t
      treemacs-silent-refresh t
      treemacs-silent-filewatch t
      treemacs-show-hidden-files t
      treemacs-is-never-other-window t
      treemacs-user-mode-line-format 'none
      treemacs-is-never-other-window t))
  :bind (("C-c t t" . treemacs)
          ("C-c t d" . treemacs-select-directory)
          ("C-c t C-f" . treemacs-find-file)
          ("C-c t 1" . treemacs-select-window)
          ("C-c t 2" . treemacs-delete-other-windows)))

(use-package treemacs-projectile
  :after projectile
  :bind (:map projectile-command-map
          ("h" . treemacs-projectile)))

(use-package treemacs-magit
  :after magit
  :commands treemacs-magit--schedule-update
  :hook ((magit-post-commit
           git-commit-post-finish
           magit-post-stage
           magit-post-unstage)
          . treemacs-magit--schedule-update))

(use-package treemacs-all-the-icons
  :after all-the-icons)

(use-package treemacs-icons-dired
  :after (dired)
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :config (treemacs-icons-dired-mode))


(setq-default speedbar t)
(setq speedbar-show-unknown-files t)

(provide 'init-treemacs)
;;; init-treemacs.el ends here
