;; -*- lexical-binding: t; -*-

;; useful for debugging themes
(use-package fontify-face
  :commands (fontify-face-mode))

(use-package kaolin-themes
  :defer t
  :after all-the-icons
  :config
  (setq kaolin-themes-modeline-border nil)
  (kaolin-treemacs-theme))

(use-package doom-themes
  :defer t
  :config
  ;; https://github.com/doomemacs/themes/issues/875#issuecomment-3557150380
  (setcdr (assoc 'gnus-group-news-low-empty doom-themes-base-faces)
          '(:inherit 'gnus-group-mail-1-empty :weight 'normal))
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil)
  ;; (load-theme 'doom-meltbus t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)

  (defun khz/doom-meltbus-minibuffer-match-faces (theme)
    "Improve minibuffer match contrast for doom-meltbus."
    (when (eq theme 'doom-meltbus)
      (dolist (face '(orderless-match-face-0
                      orderless-match-face-1
                      orderless-match-face-2
                      orderless-match-face-3
                      consult-highlight-match
                      consult-preview-match
                      completions-common-part))
        (when (facep face)
          (set-face-attribute face nil
                              :foreground "#ffffff"
                              :background "#27515c"
                              :weight 'bold)))))

  (add-hook 'enable-theme-functions #'khz/doom-meltbus-minibuffer-match-faces)

  (with-eval-after-load 'orderless
    (when (memq 'doom-meltbus custom-enabled-themes)
      (khz/doom-meltbus-minibuffer-match-faces 'doom-meltbus)))

  (with-eval-after-load 'consult
    (when (memq 'doom-meltbus custom-enabled-themes)
      (khz/doom-meltbus-minibuffer-match-faces 'doom-meltbus)))

  (when (display-graphic-p)
    (setq doom-themes-treemacs-theme "doom-atom")
    (doom-themes-treemacs-config)))

(defvar khz/ivory-themes-directories
  (mapcar #'expand-file-name
          '("~/Code/ivory-themes"
            "~/Code/personal/ivory-themes"))
  "Candidate local Ivory theme checkouts across machines.")

(defvar khz/ivory-themes-directory
  (catch 'directory
    (dolist (directory khz/ivory-themes-directories)
      (when (file-exists-p (expand-file-name "ivory-themes.el" directory))
        (throw 'directory directory))))
  "Local Ivory theme checkout, when present on this machine.")

(when khz/ivory-themes-directory
  (add-to-list 'load-path khz/ivory-themes-directory)
  (add-to-list 'custom-theme-load-path khz/ivory-themes-directory))

(use-package ivory-themes
  :straight nil
  :if khz/ivory-themes-directory
  :commands (ivory-themes-load ivory-themes-toggle)
  :custom
  (ivory-themes-bold-constructs t)
  (ivory-themes-italic-constructs nil)
  :bind ("<f6>" . ivory-themes-toggle))

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

(use-package color-theme-sanityinc-tomorrow
  :defer t
  :straight (:type git :host github :repo "purcell/color-theme-sanityinc-tomorrow"))

(use-package tomorrow-night-deepblue-theme
  :defer t
  :straight (:type git :host github :repo "jamescherti/emacs-tomorrow-night-deepblue-theme"))

(use-package almost-mono-themes
  :defer t
  :straight (:type git :host github :repo "khzaw/almost-mono-themes"))

(use-package green-phosphor-theme
  :defer t
  :straight (:type git :host github :repo "emacsmirror/green-phosphor-theme"))

(use-package catppuccin-theme
  :straight (:type git :host github :repo "catppuccin/emacs")
  :defer t
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
  :straight (:type git :host github :repo "jaredgorski/nothing.el")
  :defer t)

(use-package sexy-monochrome-theme
  :defer t
  :straight (:type git :host github :repo "voloyev/sexy-monochrome-theme"))

(use-package modus-themes :defer t)

(use-package ef-themes :defer t)

(use-package doric-themes
  :defer t
  :straight (:type git :host github :repo "protesilaos/doric-themes"))

(use-package acme-theme
  :defer t
  :straight (:type git :host github :repo "ianyepan/acme-emacs-theme")
  :config (setq acme-theme-black-fg t))

(use-package alabaster-themes
  :disabled t
  :defer t
  :straight (:type git :host github :repo "vedang/alabaster-themes")
  :init
  (setcdr (assoc 'gnus-group-news-low-empty doom-themes-base-faces)
          '(:inherit 'gnus-group-mail-1-empty :weight 'normal)))

(use-package ember-theme
  :defer t
  :straight (:type git :host github :repo "ember-theme/emacs" :local-repo "ember-theme")
  :config (add-to-list 'custom-theme-load-path
                       (file-name-directory (locate-library "ember-theme"))))

(use-package nordic-night-theme
  :defer t
  :straight (:type git :host codeberg :repo "ashton314/nordic-night" :branch "main")
  :init (load-theme 'nordic-midnight t))

(provide 'init-themes)
;; init-themes.el ends here
