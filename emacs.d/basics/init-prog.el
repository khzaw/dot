;;; package --- init-prog.el
;;; Commentary:

;;; Code:
(use-package prog-mode
  :ensure nil
  :config
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (global-prettify-symbols-mode t)
  :hook (prog-mode . prettify-symbols-mode))

(electric-pair-mode 1)

(use-package paren
  :init (show-paren-mode))

(use-package smartparens
  :commands (smartparens-mode smartparens-strict-mode)
  :config (require 'smartparens-config))

(provide 'init-prog)
;;; init-prog.el ends here
