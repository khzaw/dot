;; -*- lexical-binding: t; -*-
(use-package solo-jazz-theme)

(use-package kaolin-themes
  :after all-the-icons
  :config
  (setq kaolin-themes-modeline-border nil)
  (kaolin-treemacs-theme))

(use-package doom-themes
  :config (setq doom-themes-enable-italic nil
                doom-themes-enable-bold t)
  (doom-themes-visual-bell-config)
  (if (display-graphic-p)
      (progn
        (setq doom-themes-treemacs-theme "doom-atom")
        (doom-themes-treemacs-config)))
  (doom-themes-org-config))

(use-package modus-themes
  :config
  (setq modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-completions '((t . (extrabold)))
        ;; Keep the border of mode line but make it the same color as the background of the mode line
        modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          (fringe unspecified)))

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle)

  (defun my-modus-themes-invisible-dividers (_theme)
    "Make window dividers for THEME invisible."
    (let ((bg (face-background 'default)))
      (custom-set-faces
       `(fringe ((t :background ,bg :foreground ,bg)))
       `(window-divider ((t :background ,bg :foreground ,bg)))
       `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
       `(window-divider-last-pixel ((t :background ,bg :foreground ,bg))))))
  (add-hook 'enable-theme-functions #'my-modus-themes-invisible-dividers))

(use-package ef-themes)

(use-package color-theme-sanityinc-tomorrow
  :straight (:type git :host github :repo "purcell/color-theme-sanityinc-tomorrow"))

(use-package tomorrow-night-deepblue-theme
  :straight (:type git :host github :repo "jamescherti/emacs-tomorrow-night-deepblue-theme"))

(use-package almost-mono-themes
  :straight (:type git :host github :repo "khzaw/almost-mono-themes"))
;; (custom-set-faces
;;  `(shadow ((t :foreground "#cccccc")))))

(use-package grandshell-theme)

(use-package green-phosphor-theme
  :straight (:type git :host github :repo "emacsmirror/green-phosphor-theme"))

(use-package miasma-theme)

(use-package fleetish-theme)

(use-package tok-theme)

(use-package parchment-theme)

(use-package alect-themes
  :straight (:type git :host github :repo "alezost/alect-themes"))

(use-package faff-theme
  :straight (:type git :host github :repo "WJCFerguson/emacs-faff-theme"))

(use-package catppuccin-theme
  :straight (:type git :host github :repo "catppuccin/emacs")
  :config
  (defun my-apply-catppuccin-vertico-face (theme)
    "Apply custom vertico-current background if THEME is 'catppuccin."
    (when (and (eq theme 'catppuccin) (facep 'vertico-current))
      (let ((vertico-bg (catppuccin-color 'surface1)))
        (when vertico-bg ; Ensure the color name exists and returned a value
          (set-face-attribute 'vertico-current nil :background vertico-bg)))))

  (defun my-reset-catppuccin-vertico-face (theme)
    "Reset vertico-current background if THEME being disabled is 'catppuccin."
    (when (and (eq theme 'catppuccin) (facep 'vertico-current))
      (set-face-attribute 'vertico-current nil :background nil)))

  (add-hook 'enable-theme-functions #'my-apply-catppuccin-vertico-face)
  (add-hook 'disable-theme-functions #'my-reset-catppuccin-vertico-face)

  (setq catppuccin-flavor 'mocha)
  ;; `Overlay0` is too dim AA (3.35:1). `Overlay2` on base is (5.81:1).
  ;; but `Overlay2` is too bright for a comment.
  ;; Using `Subtext0` will get AAA, but it too similar to `Text`.
  (catppuccin-set-color 'overlay0 "#7f849c" 'mocha)

  ;; Catppuccin black.
  (catppuccin-set-color 'base "#000000" 'mocha)
  (catppuccin-set-color 'mantle "#000000" 'mocha)
  (catppuccin-set-color 'crust "#000000" 'mocha)

  (load-theme 'catppuccin t)
  (catppuccin-reload))


(provide 'init-themes)
;; init-themes.el ends here
