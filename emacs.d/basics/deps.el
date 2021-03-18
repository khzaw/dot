(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setenv "SHELL" "/usr/local/bin/zsh")
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.4))

(use-package editorconfig :ensure t :config (editorconfig-mode 1))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package undo-tree
  :ensure t
  :diminish 
  :hook
  (after-init . global-undo-tree-mode))

(use-package eldoc
  :ensure t
  :diminish eldoc-mode)

(use-package avy
  :ensure t
  :bind (("C-;"   . avy-goto-char)
         ("C-'"   . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-0)
         ("M-g e" . avy-goto-word-1))
  :hook (after-init . avy-setup-default)
  :config (setq avy-background t
                avy-all-windows nil
                avy-all-windows-alt t
                avy-style 'pre))

(use-package rjsx-mode
  :ensure t
  :defer t
  :mode ("\\.jsx?\\'" . rjsx-mode)
  :hook
  (rjsx-mode  setup-tide-mode-hook))

(use-package all-the-icons
  :ensure t
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.tsx$"
                 all-the-icons-fileicon "tsx"
                 :height 1.0
                 :v-adjust -0.1
                 :face all-the-icons-cyan-alt)))


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

(use-package go-mode
  :ensure t)

(use-package go-playground
  :diminish
  :commands (go-playground-mode))

(use-package yaml-mode
  :ensure t)

(use-package css-mode

(use-package ace-window
  :ensure t)

(use-package elogt
  :ensure t
  :hook
    (go-mode . eglot-ensure))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-monokai-pro t))


(provide 'deps)
