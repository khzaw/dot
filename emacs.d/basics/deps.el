(use-package evil
             :ensure t
             :init
             (setq evil-want-keybinding nil)
             :config
             (evil-set-initial-state 'help-mode 'emacs)
             (evil-set-initial-state 'dashboard-mode 'emacs)
             (evil-set-initial-state 'fundamental-mode 'emacs)
             (evil-mode 1))
(use-package evil-escape
             :ensure t
             :init (setq-default evil-escape-key-sequence "kj")
             :config
             (evil-escape-mode 1))
(use-package evil-collection
             :ensure t
             :after evil
             :config
             (evil-collection-init))


(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t))


(provide 'deps)
