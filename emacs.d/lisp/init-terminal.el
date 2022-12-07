(use-package vterm
  :defer t
  :bind (:map vterm-mode-map
          ("s-t" . vterm) ; open up new tabs quickly
          ("C-\\" . popper-cycle)))

(use-package eshell-syntax-highlighting
  :after (esh-mode eshell)
  :defer t
  :config (eshell-syntax-highlighting-global-mode +1))

(provide 'init-terminal)
;;; init-terminal.el ends here
