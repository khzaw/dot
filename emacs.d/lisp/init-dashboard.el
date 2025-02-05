;; -*- lexical-binding: t; -*-

(use-package dashboard
  :straight (:type git :host github :repo "emacs-dashboard/emacs-dashboard")
  :custom
  (dashboard-banner-logo-title "Create illusions of effortless competence.")
  (dashboard-items '((recents . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)))
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-image-banner-max-width 400)
  (dashboard-set-navigator t)
  (dashboard-navigation-cycle t)
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-week-agenda t)
  :config
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook #'hide-mode-line-mode))

(provide 'init-dashboard)
