(use-package vterm)

(use-package vterm-toggle
  :custom
  (vterm-toggle-scope 'project)
  :bind (("C-c t o" . #'vterm-toggle)
          :map vterm-mode-map
          ("s-t" . #'vterm) ; Open up new tabs quickly
          ("C-\\" . #'popper-cycle)))

(provide 'init-terminal)
;;; init-terminal.el ends here
