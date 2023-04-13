(use-package solo-jazz-theme)

(use-package humanoid-themes)

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
    modus-themes-mail-citations 'intense
    modus-themes-subtle-line-numbers t)
  (load-theme 'modus-vivendi t)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package ef-themes)

(use-package almost-mono-themes)

(use-package bespoke-themes
  :straight (:host github :repo "mclear-tools/bespoke-themes")
  :config
  (setq bespoke-set-evil-cursors t)
  (setq bespoke-set-italic-comments t)
  (setq bespoke-set-italic-keywords t)
  (setq bespoke-set-variable-pitch t)
  (setq bespoke-set-theme 'dark)
  ;; (load-theme 'bespoke t)
  )

(use-package stimmung-themes
  :straight (stimmung-themes :host github :repo "motform/stimmung-themes") ; if you are a straight shooter
  ;; (stimmung-themes-load-dark)
  ) ; or (stimmung-themes-load-dark)

(use-package nano-theme
  :straight (nano-theme
              :type git
              :host github
              :repo "rougier/nano-theme"))

(use-package timu-caribbean-theme)

(provide 'init-themes)
