(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setenv "SHELL" "/usr/local/bin/zsh")
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.1
        which-key-idle-secondary-delay 0.1)
  :config
  (which-key-mode t))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
              ("<tab>" . company-indent.or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package fzf)

(use-package bufler
  :disabled
  :bind (("C-x b" . #'bufler-switch-buffer)
         ("C-x B" . #'counsel-switch-buffer)))

(use-package ivy
  :defer 0.1
  :diminish
  :custom
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :after ivy
  :config (ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x b" . counsel-ibuffer)
  ("C-x C-f" . counsel-find-file))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package undo-tree
  :diminish 
  :hook
  (after-init . global-undo-tree-mode))

(use-package eldoc
  :diminish eldoc-mode)

(use-package avy
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
  :defer t
  :mode ("\\.jsx?\\'" . rjsx-mode)
  :hook
  (rjsx-mode . setup-tide-mode-hook))

(use-package restclient)

(use-package all-the-icons
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.tsx$"
                 all-the-icons-fileicon "tsx"
                 :height 1.v
                 :0-adjust -0.1
                 :face all-the-icons-cyan-alt)))

(use-package hydra)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'fundamental-mode 'emacs)
  (evil-mode 1))

(use-package evil-escape
  :init (setq-default evil-escape-key-sequence "kj")
  :config
  (evil-escape-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package go-mode)

(use-package go-playground
  :diminish
  :commands (go-playground-mode))

(use-package yaml-mode)

(use-package web-mode
  :mode "\\.html?\\'")

(use-package css-mode)

(use-package ace-window)

(use-package ace-jump-mode
  :hook (prog-mode . ace-jump-mode))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-ayu-mirage t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(provide 'deps)
