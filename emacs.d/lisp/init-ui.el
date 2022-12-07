;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)

(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; UI
(setq default-frame-alist
  (append (list
	          '(min-height . 1)  '(height     . 45)
	          '(min-width  . 1)  '(width      . 90))))
(setq-default line-spacing 2)
;; (set-frame-font "JetBrains Mono 14" nil t)
(set-face-attribute 'default nil :font "Inconsolata" :weight 'normal :height 180)
(set-face-attribute 'fixed-pitch nil :font "Inconsolata" :weight 'normal :height 180)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'normal :height 1.25)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
(setq x-underline-at-descent-line t)

(defun transparency (value)
  "Set the transparency of the frame window to VALUE 0=transparent/100=opaque."
  (interactive "nTransparency Value (0 - 100) :")
  (set-frame-parameter (selected-frame) 'alpha value))
(transparency 94)

(use-package solaire-mode
  :straight t
  :hook
  ;; Ensure solaire-mode is running in all solaire-mode buffers
  (after-load-theme . solaire-global-mode)
  (change-major-mode . turn-on-solaire-mode)
  (after-revert . turn-on-solaire-mode)
  (ediff-prepare-buffer . solaire-mode)
  :config
  (solaire-global-mode +1))

(use-package all-the-icons :if (display-graphic-p))

(use-package kaolin-themes
  :after all-the-icons
  :config
  (kaolin-treemacs-theme))

(use-package doom-themes
  :config
  (load-theme 'doom-pine t))

(use-package modus-themes
  :config
  (setq modus-themes-fringes nil))

(use-package ef-themes)

(use-package sweet-theme)

(use-package almost-mono-themes)

(use-package minions)

;; Show native line numbers if possible, otherwise use `linum'
(if (fboundp 'display-line-numbers-mode)
  (use-package display-line-numbers
    :ensure nil
    ;; :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
    :init (setq display-line-numbers-width-start t))
  (use-package linum-off
    :demand t
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :init (setq linum-format "%4d ")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)

;; Display dividers between windows
(setq window-divider-default-places t
  window-divider-default-bottom-width 1
  window-divider-default-right-width 1)
;; (add-hook 'window-setup-hook #'window-divider-mode)

;; Easily adjust the font size in all frames
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

(use-package good-scroll
      :diminish
      :hook (after-init . good-scroll-mode)
      :bind (([remap next] . good-scroll-up-full-screen)
             ([remap prior] . good-scroll-down-full-screen)))

;; Smooth scrolling over images
(use-package iscroll
  :diminish
  :hook (image-mode . iscroll-mode))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch :diminish)

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

(use-package posframe
  :hook ((after-load-theme . posframe-delete-all)))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(use-package beacon
  :diminish
  :config (beacon-mode 1))

(use-package mood-line :config (mood-line-mode))

(use-package chronos)

(use-package ansi-color
  ;; ANSI coloring in compilation buffer
  :hook (compilation-filter-hook . ansi-color-compilation-filter))

(use-package redacted :bind ("C-c C-d" . redacted-mode))

(use-package focus :defer t)

(use-package centaur-tabs
  :init
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-gray-out-icons 'buffer)
  :config
  (centaur-tabs-mode t)
  :bind
  (:map evil-normal-state-map
    ("g t" . centaur-tabs-forward)
    ("g T" . centaur-tabs-backward)))

(use-package autothemer)

(use-package catppuccin-theme)

;; Makes manual pages nicer to look at
(use-package info-colors :commands (info-colors-fontify-node)
  :hook (Info-selection-hook . info-colors-fontify-node))

(use-package theme-magic)

;; set discord status
(use-package elcord
  :commands elcord-mode
  :config (setq elcord-use-major-mode-as-main-icon t))

;; minimap
(use-package sublimity
  :defer t
  :config
  (require 'sublimity-scroll)
  (require 'sublimity-map)
  (require 'sublimity-attractive))

(provide 'init-ui)
;;; init-ui.el ends here
