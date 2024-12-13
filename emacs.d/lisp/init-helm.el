;;; -*- lexical-binding: t; -*-

(use-package helm
  :config
  (setq helm-split-window-default-side 'below)
  (setq helm-split-window-inside-p t)
  (setq helm-scroll-amount 4)
  (setq helm-input-idle-delay 0.01))

(use-package helm-describe-modes
  :bind ([remap describe-mode] . helm-describe-modes))

(provide 'init-helm)
