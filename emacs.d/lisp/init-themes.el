
(use-package solo-jazz-theme)

(use-package humanoid-themes
  :config
  (load-theme 'humanoid-light t))

(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords nil)
  (lambda-themes-set-variable-pitch t))

(use-package catppuccin-theme)

(use-package kaolin-themes
  :after all-the-icons
  :config
  (kaolin-treemacs-theme))

(use-package doom-themes
  :config
  (setq doom-themes-enable-italic nil)
  (setq doom-themes-enable-bold t)
  (doom-themes-visual-bell-config)
  (if (display-graphic-p)
    (progn
      (setq doom-themes-treemacs-theme "doom-colors")
      (doom-themes-treemacs-config)))
  (doom-themes-org-config))

(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs nil)
  (setq modus-themes-bold-constructs t)
  (setq
    modus-themes-mixed-fonts t
    modus-themes-mode-line 'borderless
    modus-themes-mail-citations 'intense
    modus-themes-subtle-line-numbers t
    modus-themes-fringes 'subtle)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package ef-themes)

(use-package almost-mono-themes)

(provide 'init-themes)
