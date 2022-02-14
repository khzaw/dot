;;; package --- init-window.el
;;; Commentary:

;;; Code:
(use-package shackle
  :init
  (setq
    shackle-default-alignment 'below
    shackle-default-size 0.4
    shackle-rules '((help-mode :align below :select t)
                     (helpful-mode :align below)
                     (compilation-mode :select t :size 0.25)
                     ("*compilation*" :select nil :size 0.25)
                     ("*ag search*" :select nil :size 0.25)
                     ("*Flycheck errors*" :select nil :size 0.25)
                     ("*Warnings*" :select nil :size 0.25)
                     ("*Error*" :select nil :size 0.25)
                     (magit-status-mode :align bottom :size 0.5 :inhibit-window-quit t)
                     (magit-log-mode :same t :inhibit-window-quit t)
                     (magit-commit-mode :ignore t)
                     (magit-diff-mode :select nil :align left :size 0.5)
                     (git-commit-mode :same t)
                     (vc-annotate-mode :same t)))
  :config
  (shackle-mode 1))

(use-package popper
  :bind (("C-\\"    . popper-toggle-latest)
          ("M-\\"   . popper-cycle)
          ("C-M-\\" . popper-toggle-type))
  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  :custom
  (popper-reference-buffers '("\\*Messages\\*"
                               "Output\\*$"
                               "\\*Async Shell Command\\*"
                               "\\*xref\\*"
                               "\\*compilation\\*"
                               help-mode
                               "\\*Warnings\\*"
                               compilation-mode)))

(provide 'init-window)
;;; init-window.el ends here
