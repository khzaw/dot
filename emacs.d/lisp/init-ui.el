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
	          '(font . "JetBrains Mono:size=14")
	          '(min-height . 1)  '(height     . 45)
	          '(min-width  . 1)  '(width      . 90))))
(setq-default line-spacing 2)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
(setq x-underline-at-descent-line t)

(use-package solaire-mode
  :defer 1
  :hook
  ;; Ensure solaire-mode is running in all solaire-mode buffers
  (after-load-theme . solaire-global-mode)
  (change-major-mode . turn-on-solaire-mode)
  (after-revert . turn-on-solaire-mode)
  (ediff-prepare-buffer . solaire-mode)
  :custom
  (solaire-mode-auto-swap-bg t)
  :config
  (solaire-global-mode +1))

(use-package doom-themes
  :custom (doom-themes-treemacs-theme "doom-colors")
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  (setq doom-themes-treemacs-variable-pitch-face nil)
  (with-eval-after-load 'lsp-treemacs
    (doom-themes-treemacs-config))
  (doom-themes-org-config))

(use-package all-the-icons :if (display-graphic-p))

(use-package kaolin-themes
  :after all-the-icons
  :config
  (load-theme 'kaolin-dark t)
  (kaolin-treemacs-theme))

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
(add-hook 'window-setup-hook #'window-divider-mode)

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

;; Don't open a file in a new frame
(setq ns-popup-frames nil)

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(use-package beacon
  :diminish
  :config (beacon-mode 1))

(provide 'init-ui)
;;; init-ui.el ends here
