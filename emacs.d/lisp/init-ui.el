;; -*- lexical-binding: t; -*-
(setq which-func-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)

(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq redisplay-skip-fontification-on-input t)

;; (toggle-frame-maximized)

;; UI
(when (eq system-type 'gnu/linux)
  (setq-default line-spacing 0.05)
  (set-face-attribute 'default nil :font "Berkeley Mono" :weight 'medium :height 90)
  (set-face-attribute 'fixed-pitch nil :font "Berkeley Mono" :weight 'normal :height 1.0)
  (set-face-attribute 'variable-pitch nil :font "Berkeley Mono" :weight 'normal))

(when (eq system-type 'darwin)
  (setq-default line-spacing -0.1)
  (set-face-attribute 'default nil :font "Berkeley Mono" :weight 'normal :height 150)
  (set-face-attribute 'fixed-pitch nil :font "Berkeley Mono" :weight 'normal :height 1.0)
  (set-face-attribute 'variable-pitch nil :font "Iosevka" :weight 'normal :height 1.0))

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
  :hook (after-init . global-page-break-lines-mode))

(use-package posframe
  :disabled t
  :hook ((after-load-theme . posframe-delete-all)))

(use-package posframe-plus
  :straight (:type git :host github :repo "zbelial/posframe-plus")
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

(if (file-directory-p "~/Code/elcord")
    (use-package elcord
      :load-path "~/Code/elcord"
      :commands elcord-mode
      :config (setq elcord-use-major-mode-as-main-icon t)
      (elcord-mode t))
  (use-package elcord
    ;; set discord status
    :commands elcord-mode
    :config (setq elcord-use-major-mode-as-main-icon t)
    (elcord-mode t)))


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

(use-package pulsar
  :disabled t
  :hook
  (after-init . pulsar-global-mode)
  (minibuffer-setup . pulsar-pulse-line)
  (next-error . pulsar-pulse-line-red)
  (next-error . pulsar-reveal-entry)
  (next-error . pulsar-recenter-center)
  :custom
  (pulsar-delay 0.05)
  (pulsar-iterations 13)
  :config
  (add-to-list 'pulsar-pulse-functions 'ace-window)
  (add-to-list 'pulsar-pulse-functions 'avy-goto-line)
  (add-to-list 'pulsar-pulse-functions 'beginning-of-buffer)
  (add-to-list 'pulsar-pulse-functions 'beginning-of-defun)
  (add-to-list 'pulsar-pulse-functions 'diff-hunk-next)
  (add-to-list 'pulsar-pulse-functions 'diff-hunk-prev)
  (add-to-list 'pulsar-pulse-functions 'end-of-buffer)
  (add-to-list 'pulsar-pulse-functions 'end-of-defun)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'magit-section-backward)
  (add-to-list 'pulsar-pulse-functions 'magit-section-forward)
  (add-to-list 'pulsar-pulse-functions 'isearch-repeat-backward)
  (add-to-list 'pulsar-pulse-functions 'isearch-repeat-forward)
  (add-to-list 'pulsar-pulse-functions 'other-frame)
  (add-to-list 'pulsar-pulse-functions 'treesit-beginning-of-defun)
  (add-to-list 'pulsar-pulse-functions 'treesit-end-of-defun)
  (add-to-list 'pulsar-pulse-functions 'xref-find-definitions)
  (add-to-list 'pulsar-pulse-functions 'xref-go-back)
  (add-to-list 'pulsar-pulse-functions 'xref-go-forward)
  (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
  (add-to-list 'pulsar-pulse-functions 'evil-scroll-up)
  (add-to-list 'pulsar-pulse-functions 'evil-window-down)
  (add-to-list 'pulsar-pulse-functions 'evil-window-up)
  (add-to-list 'pulsar-pulse-functions 'evil-window-left)
  (add-to-list 'pulsar-pulse-functions 'evil-window-right)
  (add-to-list 'pulsar-pulse-functions 'evil-window-next))

(use-package sideline-blame)

(use-package sideline-flymake)

(use-package sideline-lsp :after lsp-mode)

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup))

(use-package sideline
  :after (evil evil-leader)
  :config
  (setq sideline-display-backend-name t)
  (progn
    (evil-leader/set-key "s" 'sideline-mode)))

(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        (list :mode-line-width 4
              :tab-width 0
              :right-divider-width 0))
  (setq spacious-padding-subtle-mode-line
        '(:mode-line-active shadow :mode-line-inactive line-number))
  (spacious-padding-mode t))

(use-package quick-peek
  :straight (:type git :host github :repo "cpitclaudel/quick-peek")
  :commands (quick-peek-show))

(use-package peek
  :straight (:host sourcehut :repo "meow_king/peek")
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
(keymap-global-set "C-M-8" (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(keymap-global-set "C-M-9" (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(keymap-global-set "C-M-0" (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(set-frame-parameter nil 'alpha '(96 96))
(global-set-key (kbd "C-c M-t C-t") 'set-frame-alpha)

(use-package holo-layer
  :disabled t
  :straight (holo-layer :type git :host github :repo "manateelazycat/holo-layer" :files ("*" (:exclude (".git" "test"))))
  :commands holo-layer-enable
  :hook (after-init . holo-layer-enable)
  :init
  (setq holo-layer-enable-cursor-animation t))

(use-package show-font)

(use-package logos
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

(use-package lin)

(provide 'init-ui)
;;; init-ui.el ends here
