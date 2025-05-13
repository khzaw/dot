;;; -*- lexical-binding: t; -*-

(use-package helm
  :config
  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
  (setq helm-split-window-default-side 'below)
  (setq helm-split-window-inside-p t
        helm-display-buffer-default-height 10 ; Limit height if a window does appear
        helm-autoresize-max-height 0 ; Prevent resizing beyond a certain point
        helm-autoresize-min-height 1)
  (setq helm-scroll-amount 4)
  (setq helm-input-idle-delay 0.01
        helm-follow-mode-persistent t
        helm-dwim-target 'completion
        helm-grep-save-buffer-name-no-confirm t
        helm-window-prefer-horizontal-split 'decide))

(use-package helm-describe-modes
  :bind ([remap describe-mode] . helm-describe-modes))

(provide 'init-helm)
