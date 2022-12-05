(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package flycheck-posframe
  :after (flycheck posframe)
  :hook (global-flycheck-mode . flycheck-posframe-mode)
  :init
  (setq flycheck-posframe-border-width 1)
  :config
  (flycheck-posframe-configure-pretty-defaults)
  (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)
  (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
