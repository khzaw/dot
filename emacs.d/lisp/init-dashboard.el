(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Create illusions of effortless competence."
    dashboard-startup-banner 'logo
    dashboard-items '((recents . 7)
                       (bookmarks . 5)
                       (projects . 7)
                       (agenda . 5)
                       (registers . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
