;;; package --- init-prog.el
;;; Commentary:

;;; Code:
(use-package prog-mode
  :ensure nil
  :config
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (global-prettify-symbols-mode t)
  :hook (prog-mode . prettify-symbols-mode))

(use-package paren
  :init (show-paren-mode))

(use-package smartparens
  :init (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(provide 'init-prog)
;;; init-prog.el ends here
