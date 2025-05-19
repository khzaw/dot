;; -*- lexical-binding: t; -*-

(use-package eshell
  :defer t
  :config
  (defun khz/reset-scrolling-vars-for-term ()
    "Locally reset scrolling behavior in term-like buffers"
    (setq-local scroll-conservatively 0)
    (setq-local scroll-margin 0))
  (add-hook 'term-mode-hook #'khz/reset-scrolling-vars-for-term)
  (add-hook 'eshell-mode-hook #'khz/reset-scrolling-vars-for-term))

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
  :disabled t
  :straight
  (:type git
   :host codeberg
   :repo "akib/emacs-eat"
   :files ("*.el" ("term" "term/*.el") "*.texi"
           "*.ti" ("terminfo/e" "terminfo/e/*")
           ("terminfo/65" "terminfo/65/*")
           ("integration" "integration/*")
           (:exclude ".dir-locals.el" "*-tests.el"))))


(use-package toggle-term
  :straight (toggle-term :type git :host github :repo "justinlime/toggle-term.el")
  ;; :bind (("M-o f" . toggle-term-find)
  ;;        ("M-o t" . toggle-term-term)
  ;;        ("M-o s" . toggle-term-shell)
  ;;        ("M-o e" . toggle-term-eshell)
  ;;        ("M-o i" . toggle-term-ielm)
  ;;        ("M-o o" . toggle-term-toggle))
  :config
  (setq toggle-term-size 25)
  (setq toggle-term-switch-upon-toggle t))

(provide 'init-terminal)
;;; init-terminal.el ends here
