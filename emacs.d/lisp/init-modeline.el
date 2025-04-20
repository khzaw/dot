;; -*- lexical-binding: t; -*-

(display-time-mode 1) ;; display time in modeline

(use-package doom-modeline
  :disabled t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-indent-info nil
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq mode-line-right-align-edge 'right-fringe))

(use-package hide-mode-line)

(use-package moody
  :disabled t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package telephone-line
  :disabled t
  :init
  (setq telephone-line-primary-left-separator 'telephone-line-identity-left
        telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left
        telephone-line-primary-right-separator 'telephone-line-identity-right
        telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)
  (telephone-line-defsegment s1 () "Emacs")
  (telephone-line-defsegment s2 () "λ")
  (setq telephone-line-lhs
        '((accent . (s1))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil . (telephone-line-projectile-segment
                  telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil . (telephone-line-flycheck-segment
                  telephone-line-flymake-segment
                  telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil . (s2))))
  (setq telephone-line-height 24)
  (telephone-line-mode 1))

(use-package mood-line
  :config
  (setq mood-line-format mood-line-format-default)
  (setq mood-line-glyph-alist mood-line-glyphs-ascii)
  (mood-line-mode 1))

(provide 'init-modeline)
