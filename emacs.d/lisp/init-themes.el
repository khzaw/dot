(use-package solo-jazz-theme)

(use-package kaolin-themes
  :after all-the-icons
  :config
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
  :defer
  :config
  (setq
   modus-themes-bold-constructs t
   modus-themes-mixed-fonts t
   modus-themes-mail-citations 'intense
   modus-themes-subtle-line-numbers t
   modus-themes-completions '((t . (extrabold)))
   ;; Keep the border of mode line but make it the same color as the background of the mode line
   modus-themes-common-palette-overrides
   '(
     (border-mode-line-active bg-mode-line-active)
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

(use-package standard-themes
  :config
  (setq standard-themes-fringes nil))

(use-package stimmung-themes
  :straight (stimmung-themes :host github :repo "motform/stimmung-themes"))

(use-package timu-caribbean-theme)

(use-package kanagawa
  :straight (kanagawa :type git :host github :repo "konrad1977/emacs"
              :local-repo "konrad1977-emacs"
              :files (:defaults "themes/*")))


(use-package os1-theme
  :straight (:type git :host github :repo "sashimacs/os1-theme"))

(use-package hima-theme
  :straight (:type git :host github :repo "meain/hima-theme"))

(use-package nordic-night-theme
  :straight (:type git :repo "https://git.sr.ht/~ashton314/nordic-night" :branch "main"))

(use-package badwolf
  :straight (:host github :repo "bkruczyk/badwolf-emacs"))

(use-package circadian
  :config
  (setq circadian-themes '(("8:00" . modus-operandi)
                           ("19:00" . kaolin-galaxy)))
  (circadian-setup))

(use-package solarized-emacs
  :straight (:type git :host github :repo "bbatsov/solarized-emacs"))

(provide 'init-themes)
