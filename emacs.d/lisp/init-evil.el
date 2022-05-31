(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  :config
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'fundamental-mode 'emacs)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-leader
  :commands (evil-leader-mode)
  :init (global-evil-leader-mode)
  :config
  (progn
  (evil-leader/set-leader ",")
  (evil-leader/set-key "f" 'isearch-forward)
  (evil-leader/set-key "b" 'counsel-ibuffer)))

(use-package evil-escape
  :after (evil evil-collection)
  :init (setq-default evil-escape-key-sequence "kj")
  :config (evil-escape-mode 1))

(provide 'init-evil)

;;; init-evil.el ends here
