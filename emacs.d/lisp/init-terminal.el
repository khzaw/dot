(use-package vterm
  :defer t
  :bind (:map vterm-mode-map
         ("s-t" . vterm) ; open up new tabs quickly
         ("C-\\" . popper-cycle)))

(use-package eshell-syntax-highlighting
  :after (esh-mode eshell)
  :defer t
  :config (eshell-syntax-highlighting-global-mode +1))

(use-package eat
  :straight
  (:type git
   :host codeberg
   :repo "akib/emacs-eat"
   :files ("*.el" ("term" "term/*.el") "*.texi"
           "*.ti" ("terminfo/e" "terminfo/e/*")
           ("terminfo/65" "terminfo/65/*")
           ("integration" "integration/*")
           (:exclude ".dir-locals.el" "*-tests.el"))))

(provide 'init-terminal)
;;; init-terminal.el ends here
