;; -*- lexical-binding: t; -*-

(use-package symbol-overlay
  :diminish
  :hook (prog-mode . symbol-overlay-mode)
  :bind-keymap ("M-s M-s" . symbol-overlay-map)
  :bind (:map symbol-overlay-mode-map
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev))
  :commands (symbol-overlay-mode symbol-overlay-put))

(use-package symbol-overlay-mc
  :straight (:type git :host github :repo "xenodium/symbol-overlay-mc")
  :config
  (with-eval-after-load 'casual-symbol-overlay
    (symbol-overlay-mc-insert-into-casual-tmenu)))

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
              show-paren-when-point-in-periphery t)
  :config (setq show-paren-delay 0))

(use-package colorful-mode
  :straight (:type git :host github :repo "DevelopmentCool2449/colorful-mode")
  :config
  (add-to-list 'global-colorful-modes 'help-mode)
  (add-to-list 'global-colorful-modes 'helpful-mode)
  (global-colorful-mode))

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
