;; Optimization  -*- lexical-binding: t; -*-
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)

(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq redisplay-skip-fontification-on-input t)

;; UI
(toggle-frame-maximized)

(setq-default line-spacing 2)
(set-face-attribute 'default nil :font "PragmataPro Mono Liga" :weight 'normal :height 150)
(set-face-attribute 'fixed-pitch nil :font "PragmataPro Liga" :weight 'normal :height 140)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'normal :height 1.1)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
(setq x-underline-at-descent-line t)

(defun transparency (value)
  "Set the transparency of the frame window to VALUE 0=transparent/100=opaque."
  (interactive "nTransparency Value (0 - 100) :")
  (set-frame-parameter (selected-frame) 'alpha value))
(transparency 95)

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

(use-package minions)

;; Show native line numbers if possible, otherwise use `linum'
(if (fboundp 'display-line-numbers-mode)
  (use-package display-line-numbers
    :straight (:type built-in)
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
  inhibit-default-init t)


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

(use-package chronos)

(use-package ansi-color
  ;; ANSI coloring in compilation buffer
  :hook (compilation-filter-hook . ansi-color-compilation-filter))

(use-package redacted :bind ("C-c C-d" . redacted-mode))

(use-package focus :defer t)

(use-package autothemer)


;; Makes manual pages nicer to look at
(use-package info-colors :commands (info-colors-fontify-node)
  :hook (Info-selection-hook . info-colors-fontify-node))

(use-package theme-magic)

;; set discord status
(use-package elcord
  :commands elcord-mode
  :config (setq elcord-use-major-mode-as-main-icon t))

(use-package telephone-line
  :init
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
    telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
    telephone-line-primary-right-separator 'telephone-line-cubed-right
    telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-defsegment s1 () "EMACS")
  (setq telephone-line-lhs
    '((evil . (s1))
       (accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
       (nil . (telephone-line-projectile-segment
              telephone-line-buffer-segment))))
  (setq telephone-line-height 24)
  (telephone-line-mode t))

;; (use-package sideframe
;;   :straight (:type git :host github :repo "rougier/sideframe")
;;   :config
;;   (sideframe-make 'left 32)
;;   (sideframe-toggle-maximized))

;; (use-package fontset
;;   :straight (:type built-in)
;;   :config
;;   ;; Use symbola for proper unicode
;;   (when (member "Symbola" (font-family-list))
;;     (set-fontset-font
;;       t 'symbol "Symbola" nil)))

(use-package keycast)

(use-package zone
  :straight (:type built-in)
  :config
  (zone-when-idle (* 5 60)))


;; Make a clean & minimalist frame
(use-package frame
  :straight (:type built-in)
  :custom
  (window-divider-default-right-width 0)
  (window-divider-default-bottom-width 0)
  (window-divider-default-places 'right-only)
  (window-divider-mode t)
  :config
  (setq-default default-frame-alist
    (append (list
              ;; '(internal-border-width . 10)
              '(left-fringe . 0)
              '(right-fringe . 0)
              '(tool-bar-lines . 0)
              '(menu-bar-lines . 0)
              '(vertical-scroll-bars . nil))))
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t))

;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)


(use-package dimmer
  :disabled
  :commands dimmer-mode
  :config
  (setq dimmer-fraction 0.175)
  (dimmer-configure-magit)
  (dimmer-configure-posframe)

  ;; make dimmer play nicely with corfu frames
  (defun advise-dimmer-config-change-handler ()
    "Advise to only force process if no predicate is truthy."
    (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                    dimmer-prevent-dimming-predicates)))
      (unless ignore
        (when (fboundp 'dimmer-process-all)
          (dimmer-process-all t)))))

  (defun corfu-frame-p ()
    "Check if the buffer is a corfu frame buffer."
    (string-match-p "\\` \\*corfu" (buffer-name)))

  (defun dimmer-configure-corfu ()
    "Convenience settings for corfu users."
    (add-to-list
      'dimmer-prevent-dimming-predicates
      #'corfu-frame-p))

  (advice-add
    'dimmer-config-change-handler
    :override 'advise-dimmer-config-change-handler)

  (dimmer-configure-corfu)
  (dimmer-mode))


(provide 'init-ui)
;;; init-ui.el ends here
