(use-package dashboard
  :custom
  (dashboard-banner-logo-title "Create illusions of effortless competence.")
  ;; (dashboard-startup-banner (concat emacs-dir "lambda-logo-white.png"))
  (dashboard-items '((recents . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)))
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-image-banner-max-width 400)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  :config
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook #'hide-mode-line-mode))

(provide 'init-dashboard)
