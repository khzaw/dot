;; -*- lexical-binding: t; -*-

;; useful for debugging themes
(use-package fontify-face)

(use-package kaolin-themes
  :after all-the-icons
  :config
  (setq kaolin-themes-modeline-border nil)
  (kaolin-treemacs-theme))


(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil)
  (load-theme 'doom-meltbus t)

  ;; Fix Gnus cycle for meltbus
  (custom-theme-set-faces
   'doom-meltbus
   '(gnus-group-news-low ((t (:inherit gnus-group-mail-1))))
   '(gnus-group-news-low-empty ((t (:inherit gnus-group-news-low :weight normal))))
   ;; Monochrome orderless faces - these are PART of the theme
   '(orderless-match-face-0 ((t (:foreground "black" :background "#efefef" :weight bold))))
   '(orderless-match-face-1 ((t (:foreground "black" :background "#acacac" :weight bold))))
   '(orderless-match-face-2 ((t (:foreground "black" :background "#cecece" :weight bold))))
   '(orderless-match-face-3 ((t (:foreground "black" :background "#8a8a8a" :weight bold))))
   ;; Monochrome search highlights
   '(lazy-highlight ((t (:foreground "black" :background "#686868" :weight bold))))
   '(evil-ex-search ((t (:inherit isearch)))))

  (doom-themes-visual-bell-config)
  (doom-themes-org-config)

  (when (display-graphic-p)
    (setq doom-themes-treemacs-theme "doom-atom")
    (doom-themes-treemacs-config)))




;; (use-package modus-themes
;;   :straight (:type git :host github :repo "protesilaos/modus-themes")
;;   :custom
;;   (modus-themes-bold-constructs t)
;;   (modus-themes-mixed-fonts t)
;;   (modus-themes-prompts '(bold intense))
;;   (modus-themes-completions '((t . (extrabold))))
;;   ;; (modus-themes-headings my/theme-headings)
;;   :config
;;     ;; Keep the border of mode line but make it the same color as the background of the mode line
;;   (setq modus-themes-common-palette-overrides
;;         '((border-mode-line-active bg-mode-line-active)
;;           (border-mode-line-inactive bg-mode-line-inactive)
;;           (fringe unspecified)))

;;   (define-key global-map (kbd "<f5>") #'modus-themes-toggle)

;;   (defun my-modus-themes-invisible-dividers (_theme)
;;     "Make window dividers for THEME invisible."
;;     (let ((bg (face-background 'default)))
;;       (custom-set-faces
;;        `(fringe ((t :background ,bg :foreground ,bg)))
;;        `(window-divider ((t :background ,bg :foreground ,bg)))
;;        `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
;;        `(window-divider-last-pixel ((t :background ,bg :foreground ,bg))))))
;;   (add-hook 'enable-theme-functions #'my-modus-themes-invisible-dividers)
;;   ;; :hook
;;   ;; (after-init . (lambda () (load-theme 'modus-vivendi t)))
;;   )

;; (use-package modus-themes)
;; (use-package ef-themes)

;; (use-package ef-themes
;;   :init
;;   (ef-themes-take-over-modus-themes-mode 1)
;;   :config
;;   (setq modus-themes-mixed-fonts t)
;;   (setq modus-themes-bold-constructs t)
;;   (setq modus-themes-prompts '(bold intense)))

(use-package hima-theme
  :straight (:type git :host github :repo "meain/hima-theme"))

(use-package color-theme-sanityinc-tomorrow
  :straight (:type git :host github :repo "purcell/color-theme-sanityinc-tomorrow"))

(use-package tomorrow-night-deepblue-theme
  :straight (:type git :host github :repo "jamescherti/emacs-tomorrow-night-deepblue-theme"))

(use-package almost-mono-themes
    :straight (:type git :host github :repo "khzaw/almost-mono-themes"))

(use-package green-phosphor-theme
  :straight (:type git :host github :repo "emacsmirror/green-phosphor-theme"))

(use-package fleetish-theme)

(use-package tok-theme)

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

  ;; (load-theme 'catppuccin t)
  )

(use-package nothing-theme
  :straight (:type git :host github :repo "jaredgorski/nothing.el"))

(use-package berrys-theme)

(use-package sexy-monochrome-theme
  :straight (:type git :host github :repo "voloyev/sexy-monochrome-theme"))

(use-package doric-themes
  :straight (:type git :host github :repo "protesilaos/doric-themes"))

(use-package south-theme
  :straight (:type git :host github :repo "SophieBosio/south" :branch "main"))

(use-package spacemacs-theme
  :straight (:type git :host github :repo "nashamri/spacemacs-theme"))

(use-package modus-themes)

(provide 'init-themes)
;; init-themes.el ends here
