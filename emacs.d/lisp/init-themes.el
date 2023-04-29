(use-package solo-jazz-theme)

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
  (doom-themes-org-config)
  (load-theme 'doom-moonlight t))

(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs nil)
  (setq modus-themes-bold-constructs t)
  (setq
    modus-themes-mixed-fonts t
    modus-themes-mail-citations 'intense
    modus-themes-subtle-line-numbers t)
  ;; (load-theme 'modus-operandi t)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package ef-themes)

(use-package almost-mono-themes)

(use-package stimmung-themes
  :straight (stimmung-themes :host github :repo "motform/stimmung-themes")
  ;; (stimmung-themes-load-dark)
  ) ; or (stimmung-themes-load-dark)

(use-package timu-caribbean-theme)

(use-package kanagawa
  :straight (kanagawa :type git :host github :repo "konrad1977/emacs"
              :local-repo "konrad1977-emacs"
              :files (:defaults "themes/*")))

(provide 'init-themes)
