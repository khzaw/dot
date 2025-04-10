;; -*- lexical-binding: t; -*-

;; Highlight the current line
(use-package hl-line
  :disabled t
  :straight (:type built-in)
  :hook ((on-first-buffer . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

;; Highlight matching parens
(use-package paren
  :straight (:type built-in)
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t))

(use-package colorful-mode
  :straight (:type git :host github :repo "DevelopmentCool2449/colorful-mode")
  :config
  (add-to-list 'global-colorful-modes 'helpful-mode))

(use-package prism
  :diminish
  :disabled t
  :straight (prism :type git :host github :repo "alphapapa/prism.el")
  :hook ((emacs-lisp-mode clojure-mode clojurescript-mode json-mode json-ts-mode) . prism-mode))

(use-package highlight-indent-guides
  :diminish
  :hook
  ((yaml-mode yaml-ts-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-method 'character)
  :config
  (setq highlight-indent-guides-character ?\┆) ; Example: light vertical bar unicode
  (set-face-attribute 'highlight-indent-guides-odd-face nil :foreground "gray40")
  (set-face-attribute 'highlight-indent-guides-even-face nil :foreground "gray30"))

(provide 'init-highlight)
;;; init-highlight.el ends here
