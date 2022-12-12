(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "Create illusions of effortless competence."
    dashboard-startup-banner 'logo
    dashboard-items '((recents . 5)
                       (bookmarks . 5)
                       (projects . 5)
                       (agenda . 5)
                       (registers . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-set-navigator t)
  (dashboard-setup-startup-hook)
  :hook
  (dashboard-mode . hide-mode-line-mode)
  (dashboard-mode . turn-off-solaire-mode))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
