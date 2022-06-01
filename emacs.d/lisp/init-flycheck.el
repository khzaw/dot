(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode))

(use-package flycheck-posframe
  :after (flycheck posframe)
  :hook (flycheck-mode . flycheck-posframe-mode)
  :init (setq flycheck-posframe-border-width 1)
  (add-hook 'flycheck-posframe-inhibit-functions
    (lambda (&rest _) (bound-and-true-p company-backend)))
  :config
  (flycheck-posframe-configure-pretty-defaults)
  (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p)
  (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)
  (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
