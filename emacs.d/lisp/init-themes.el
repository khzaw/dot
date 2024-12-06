(use-package solo-jazz-theme)

(use-package kaolin-themes
  :after all-the-icons
  :config
  (setq kaolin-themes-modeline-border nil)
  (kaolin-treemacs-theme))

(use-package doom-themes
  :config
  (setq doom-themes-enable-italic nil
        doom-themes-enable-bold t)
  (doom-themes-visual-bell-config)
  (if (display-graphic-p)
      (progn
        (setq doom-themes-treemacs-theme "doom-colors")
        (doom-themes-treemacs-config)))
  (doom-themes-org-config))

(use-package modus-themes
  :config
  (setq
   modus-themes-bold-constructs t
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

(use-package stimmung-themes
  :straight (stimmung-themes :host github :repo "motform/stimmung-themes"))

(use-package timu-caribbean-theme)

(use-package os1-theme
  :straight (:type git :host github :repo "sashimacs/os1-theme"))

(use-package hima-theme
  :straight (:type git :host github :repo "meain/hima-theme"))

(use-package apropospriate-theme
  :straight (:type git :host github :repo "waymondo/apropospriate-theme"))

(use-package color-theme-sanityinc-tomorrow
  :straight (:type git :host github :repo "purcell/color-theme-sanityinc-tomorrow"))

(use-package tomorrow-night-deepblue-theme
  :straight (:type git :host github :repo "jamescherti/emacs-tomorrow-night-deepblue-theme")
  :config (load-theme 'tomorrow-night-deepblue t))

(use-package nano-theme
  :straight (:type git :host github :repo "rougier/nano-theme"))

(use-package almost-mono-themes
  :straight (:type git :host github :repo "khzaw/almost-mono-themes"))
;; (custom-set-faces
;;  `(shadow ((t :foreground "#cccccc")))))

(use-package acme-theme
  :straight (:type git :host github :repo "ianyepan/acme-emacs-theme"))

(use-package plan9-theme
  :straight (:type git :host github :repo "john2x/plan9-theme.el"))

(use-package grandshell-theme)

(use-package doom-kyoto-night-theme
  :after doom-themes
  :straight (:type git :host github :repo "shrikecode/doom-kyoto-night-theme"))

(use-package carbon-theme
  :straight (:type git :host github :repo "gopesh-human/carbon-theme"))

(use-package eink-theme
  :straight (:type git :host github :repo "maio/eink-emacs"))

(use-package no-clown-fiesta-theme
  :straight (no-clown-fiesta-theme :type git :host github :repo "emacsmirror/no-clown-fiesta-theme"))

(use-package flexoki-themes
  :custom
  (flexoki-themes-use-bold-keywords t)
  (flexoki-themes-use-bold-builtins t)
  (flexoki-themes-use-italic-comments t))

(use-package green-phosphor-theme
  :straight (:type git :host github :repo "emacsmirror/green-phosphor-theme"))

(provide 'init-themes)
