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

  (when (display-graphic-p)
    (setq doom-themes-treemacs-theme "doom-atom")
    (doom-themes-treemacs-config)))

(use-package ivory-themes
  :preface
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

  :straight nil
  :if khz/ivory-themes-directory
  :demand t
  :preface
  (defun khz/reload-ivory-theme (&optional theme)
    "Reload the local Ivory theme files and enable THEME."
    (interactive)
    (unless khz/ivory-themes-directory
      (user-error "No local Ivory theme checkout found"))
    (let* ((theme (or theme
                      (cond
                       ((memq 'ivory-dark custom-enabled-themes) 'ivory-dark)
                       ((memq 'ivory-light custom-enabled-themes) 'ivory-light))
                      'ivory-light))
           (theme-file (format "%s-theme.el" theme)))
      (unless (memq theme '(ivory-light ivory-dark))
        (user-error "Unknown Ivory theme: %s" theme))
      (dolist (enabled '(ivory-light ivory-dark))
        (when (memq enabled custom-enabled-themes)
          (disable-theme enabled)))
      (load-file (expand-file-name "ivory-themes.el" khz/ivory-themes-directory))
      (load-file (expand-file-name theme-file khz/ivory-themes-directory))
      (enable-theme theme)))
  :commands (ivory-themes-load ivory-themes-toggle)
  :custom
  (ivory-themes-bold-constructs t)
  (ivory-themes-italic-constructs nil)
  :bind (("<f6>" . ivory-themes-toggle)
         ("S-<f6>" . khz/reload-ivory-theme))
  :config
  (setq ivory-themes-soft-backgrounds t)
  (ivory-themes-load 'ivory-light))

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

(use-package batppuccin
  :straight (:type git :host github :repo "bbatsov/batppuccin-emacs")
  :defer t)

(use-package tokyo-night :straight (:type git :host github :repo "bbatsov/tokyo-night-emacs") :defer t)

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

(use-package nordic-night-theme
  :defer t
  :straight (:type git :host codeberg :repo "ashton314/nordic-night" :branch "main"))

(provide 'init-themes)
;; init-themes.el ends here
