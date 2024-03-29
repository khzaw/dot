
(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-char-position 'left
              aw-ignore-current nil
              aw-leading-char-style 'char
              aw-scope 'frame)
  :bind (("M-o" . ace-window)
         ("M-O" . ace-swap-window)))

(use-package shackle
  :init
  (setq
    shackle-default-alignment 'below
    shackle-default-size 0.4
    shackle-rules '((help-mode :align below :select t)
                     (helpful-mode :align below)
                     (compilation-mode :select nil :size 0.25)
                     ("*ag search*" :select nil :size 0.25)
                     ("*Warnings*" :select nil :size 0.25)
                     ("*Error*" :select nil :size 0.25)
                     (magit-status-mode :align bottom :size 0.65 :inhibit-window-quit t)
                     (magit-log-mode :same t :inhibit-window-quit t)
                     (magit-commit-mode :ignore t)
                     (magit-diff-mode :select nil :align left :size 0.5)
                     (git-commit-mode :same t)
                     (vc-annotate-mode :same t)))
  :config
  (shackle-mode 1))

(use-package popper
  :bind (("C-\\" . popper-toggle-latest)
          ("M-\\" . popper-cycle)
          ("C-M-\\" . popper-toggle-type))
  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  :custom
  (popper-reference-buffers
    '("\\*Messages\\*"
       "Output\\*$"
       "\\*Async Shell Command\\*"
       xref--xref-buffer-mode
       help-mode
       helpful-mode
       "\\*Warnings\\*"
       "\\*eldoc\\*"
       "\\*Flymake log\\*"
       "\\*xref\\*"
       "\\*Go Test\\*"
       flycheck-error-list-mode
       compilation-mode
       go-test-mode
       "^\\*vterm.*\\*$" vterm-mode
       "^\\*eshell.*\\*$" eshell-mode
       "^\\*term.*\\*$" term-mode
       "^\\*shell.*\\*$" shell-mode)))

(use-package winner-mode
  :straight (:type built-in)
  :after evil
  :hook (after-init . winner-mode)
  :bind
  (:map evil-window-map
    ("u" . winner-undo)
    ("U" . winner-redo)))

(use-package burly
  :straight (:type git :host github :repo "alphapapa/burly.el"))

(use-package zoom
  :bind (("C-c z z" . zoom))
  :config
  (setq zoom-size '(0.618 . 0.618)))

(provide 'init-window)
;;; init-window.el ends here
