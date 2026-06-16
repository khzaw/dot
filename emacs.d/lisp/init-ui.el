;; -*- lexical-binding: t; -*-
(setq which-func-update-delay 1.0)

(setq fast-but-imprecise-scrolling t)

;; prevents fontification text while typing
(setq redisplay-skip-fontification-on-input t)

;; (toggle-frame-maximized)

;; UI
(defun khz/apply-ui-fonts (&rest _)
  "Apply the default font setup after startup or theme changes."
  (pcase system-type
    ('gnu/linux
     (setq-default line-spacing 0.05)
     (set-face-attribute 'default nil :font "Berkeley Mono" :weight 'normal :height 110)
     (set-face-attribute 'fixed-pitch nil :font "Berkeley Mono" :weight 'normal :height 1.0)
     (set-face-attribute 'variable-pitch nil :font "IBM Plex Sans" :weight 'normal :height 1.0))
    ('darwin
     (setq-default line-spacing 3)
     (set-face-attribute 'default nil :font "Berkeley Mono" :weight 'normal :height 130)
     (set-face-attribute 'fixed-pitch nil :font "Berkeley Mono" :weight 'normal :height 1.0)
     (set-face-attribute 'variable-pitch nil :font "IBM Plex Sans" :weight 'normal :height 1.0))))

(khz/apply-ui-fonts)
(add-hook 'after-load-theme-hook #'khz/apply-ui-fonts)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
(setq x-underline-at-descent-line t)

(defun khz/run-with-idle-timer-after-startup (fn delay &rest args)
  "Run FN with ARGS once Emacs has been idle for DELAY seconds."
  (apply #'run-with-idle-timer delay nil fn args))

(defun khz/enable-solaire-after-startup ()
  "Enable `solaire-global-mode' after startup has settled."
  (khz/run-with-idle-timer-after-startup #'solaire-global-mode 1.0 +1))

(defun khz/enable-default-text-scale-after-startup ()
  "Enable `default-text-scale-mode' after startup has settled."
  (khz/run-with-idle-timer-after-startup #'default-text-scale-mode 1.0 +1))

(defun khz/enable-page-break-lines-after-startup ()
  "Enable `global-page-break-lines-mode' after startup has settled."
  (khz/run-with-idle-timer-after-startup #'global-page-break-lines-mode 1.0 +1))

(defun khz/enable-spacious-padding-after-startup ()
  "Enable `spacious-padding-mode' after startup has settled."
  (khz/run-with-idle-timer-after-startup #'spacious-padding-mode 1.0 +1))

(defun khz/enable-elcord-after-startup ()
  "Enable `elcord-mode' after startup has settled."
  (khz/run-with-idle-timer-after-startup #'elcord-mode 3.0 +1))

(use-package solaire-mode
  :commands (solaire-global-mode turn-on-solaire-mode solaire-mode)
  :hook
  ;; Ensure solaire-mode is running in all solaire-mode buffers
  (emacs-startup . khz/enable-solaire-after-startup)
  (after-load-theme . solaire-global-mode)
  (change-major-mode . turn-on-solaire-mode)
  (after-revert . turn-on-solaire-mode)
  (ediff-prepare-buffer . solaire-mode)
  :config
  (defun khz/turn-off-solaire-in-primary-input-buffers ()
    "Keep primary input buffers on the base theme background."
    (when (or (minibufferp)
              (string= (buffer-name) "*scratch*"))
      (turn-off-solaire-mode)))
  (add-hook 'minibuffer-setup-hook #'khz/turn-off-solaire-in-primary-input-buffers)
  (add-hook 'solaire-mode-hook #'khz/turn-off-solaire-in-primary-input-buffers)
  :defer t)

(use-package all-the-icons
  :if (display-graphic-p)
  :defer t)

(use-package minions
  :commands minions-mode
  :defer t)

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
  :commands default-text-scale-mode
  :hook (emacs-startup . khz/enable-default-text-scale-after-startup))

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
  :disabled t
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
  :commands global-page-break-lines-mode
  :hook (emacs-startup . khz/enable-page-break-lines-after-startup))

(use-package posframe
  :disabled t
  :hook ((after-load-theme . posframe-delete-all)))

(use-package posframe-plus
  :straight (:type git :host github :repo "zbelial/posframe-plus")
  :commands posframe-delete-all
  :hook ((after-load-theme . posframe-delete-all))
  :defer t)

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(use-package chronos
  :commands (chronos-add-timer))

(use-package ansi-color
  ;; ANSI coloring in compilation buffer
  :hook (compilation-filter-hook . ansi-color-compilation-filter))

(use-package redacted :bind ("C-c C-d" . redacted-mode))

(use-package focus :defer t)

(use-package autothemer
  :defer t)

;; Makes manual pages nicer to look at
(use-package info-colors :commands (info-colors-fontify-node)
  :hook (Info-selection-hook . info-colors-fontify-node))

(use-package theme-magic
  :commands (theme-magic-from-emacs))

(if (file-directory-p "~/Code/elcord")
    (use-package elcord
      :load-path "~/Code/elcord"
      :commands elcord-mode
      :hook (emacs-startup . khz/enable-elcord-after-startup)
      :custom
      (elcord-use-major-mode-as-main-icon t))
  (use-package elcord
    ;; set discord status
    :commands elcord-mode
    :hook (emacs-startup . khz/enable-elcord-after-startup)
    :custom
    (elcord-use-major-mode-as-main-icon t)))


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
                         '(tool-bar-lines . 0)
                         '(menu-bar-lines . 0)
                         '(vertical-scroll-bars . nil))))
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t))

;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)

(use-package tabspaces
  :disabled t
  :straight (:type git :host github :repo "mclear-tools/tabspaces")
  ;; :hook (after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t))

(use-package sideline-blame
  :after sideline
  :defer t)

(use-package sideline-flymake
  :after sideline
  :defer t)

(use-package sideline-lsp
  :after sideline
  :defer t)

(use-package sideline-flycheck
  :after sideline
  :defer t)

(use-package sideline
  :commands sideline-mode
  :custom
  (sideline-display-backend-name t)
  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key "s" 'sideline-mode)))

(use-package spacious-padding
  :commands spacious-padding-mode
  :hook (emacs-startup . khz/enable-spacious-padding-after-startup)
  :custom
  (spacious-padding-widths
   '(:mode-line-width 2
     :tab-width 0
     :right-divider-width 0)))

(use-package quick-peek
  :straight (:type git :host github :repo "cpitclaudel/quick-peek")
  :commands (quick-peek-show))

(use-package peek
  :straight (:host sourcehut :repo "meow_king/peek")
  :defer t
  :custom
  (peek-enable-eldoc-display-integration t))

(defun set-frame-alpha (arg &optional active)
  (interactive "nEnter alpha value (1-100): \np")
  (let* ((elt (assoc 'alpha default-frame-alist))
         (old (frame-parameter nil 'alpha))
         (new (cond ((atom old)     `(,arg ,arg))
                    ((eql 1 active) `(,arg ,(cadr old)))
                    (t              `(,(car old) ,arg)))))
    (if elt (setcdr elt new) (push `(alpha ,@new) default-frame-alist))
    (set-frame-parameter nil 'alpha new)))

(defun sanityinc/adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(defun khz/decrease-frame-alpha ()
  "Decrease the selected frame alpha."
  (interactive)
  (sanityinc/adjust-opacity nil -2))

(defun khz/increase-frame-alpha ()
  "Increase the selected frame alpha."
  (interactive)
  (sanityinc/adjust-opacity nil 2))

(defun khz/reset-frame-alpha ()
  "Reset the selected frame alpha to fully opaque."
  (interactive)
  (modify-frame-parameters nil '((alpha . 100))))

(keymap-global-set "C-M-8" #'khz/decrease-frame-alpha)
(keymap-global-set "C-M-9" #'khz/increase-frame-alpha)
(keymap-global-set "C-M-0" #'khz/reset-frame-alpha)

(defun khz/theme-dark-p ()
  "Return non-nil if the current theme has a dark background."
  (let ((bg (color-name-to-rgb (face-background 'default nil t))))
    (when bg
      (< (+ (* 0.299 (nth 0 bg))
            (* 0.587 (nth 1 bg))
            (* 0.114 (nth 2 bg)))
         0.5))))

(defun khz/adjust-alpha-for-theme (_theme)
  "Keep frames fully opaque after theme changes."
  (let ((alpha 96))
    (set-frame-parameter nil 'alpha `(,alpha ,alpha))
    (let ((elt (assoc 'alpha default-frame-alist)))
      (if elt (setcdr elt `(,alpha ,alpha))
        (push `(alpha ,alpha ,alpha) default-frame-alist)))))

(add-hook 'enable-theme-functions #'khz/adjust-alpha-for-theme)

(global-set-key (kbd "C-c M-t C-t") 'set-frame-alpha)

(use-package show-font
  :commands show-font-tabulated)

(use-package logos
  :commands (logos-focus-mode logos-narrow-dwim logos-forward-page-dwim logos-backward-page-dwim)
  :config
  ;; (setq logos-outlines-are-pages t) ;; use outlines instead of page breaks (^L)

  (defun logos-reveal-entry ()
    "Reveal Org or Outline entry."
    (cond
     ((and (eq major-mode 'org-mode)
           (org-at-heading-p))
      (org-show-subtree))
     ((or (eq major-mode 'outline-mode)
          (bound-and-true-p outline-minor-mode))
      (outline-show-subtree))))

  (defvar my-logos-no-recenter-top-modes
    '(emacs-lisp-mode lisp-interaction-mode))

  (defun my-logos-recenter-top ()
    "Use `recenter' to position the view at the top."
    (unless (memq major-mode my-logos-no-recenter-top-modes)
      (recenter 0)))

  (add-hook 'logos-page-motion-hook #'my-logos-recenter-top))

(use-package lin
  :commands lin-mode)

(provide 'init-ui)
;;; init-ui.el ends here
