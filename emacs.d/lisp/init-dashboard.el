(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "Create illusions of effortless competence."
    dashboard-startup-banner "~/Pictures/emacs_fancy_logos/xemacs_color.png"
    dashboard-items '((recents . 5)
                       (bookmarks . 5)
                       (projects . 5)
                       (agenda . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-image-banner-max-width 400)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-set-navigator t)
  (dashboard-setup-startup-hook))


(provide 'init-dashboard)
