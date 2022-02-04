;;; package --- deps.el

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config

  (when (eq system-type 'gnu/linux)
    (setenv "SHELL" "/usr/bin/zsh"))
  (when (eq system-type 'darwin)
    (setenv "SHELL" "/usr/local/bin/zsh"))
    (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
    (exec-path-from-shell-initialize))

(use-package restart-emacs)

;; (use-package aggressive-indent
;;   :diminish
;;   :hook ((after-init . global-aggressive-indent-mode)
;;           ;; Disable in big files due to performance issues
;;           (find-file . (lambda ()
;;                          (if (> (buffer-size) (* 3000 80))
;;                            (aggressive-indent-mode -1)))))
;;   :config
;;   (dolist (mode '(go-mode prolog-inferior-mode))
;;     (push mode aggressive-indent-excluded-modes))
;;   (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)
;;   (add-to-list 'aggressive-indent-dont-indent-if
;;     '(and (derived-mode-p 'c-mode 'c++mode 'csharp-mode
;;             'java-mode 'go-mode 'swift-mode)
;;        (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)" (thing-at-point 'line))))))

(use-package paredit
  :hook ((clojure-mode emacs-lisp-mode) . paredit-mode)
  :diminish (paredit paredit-mode))

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

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

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

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package rust-mode)

(use-package solidity-mode)

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

(use-package treemacs
  :commands treemacs
  :defer t
  :config
  (progn
    (treemacs-follow-mode t)))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config
  (treemacs-icons-dired-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :after (lsp-mode ivy))

(use-package lsp-treemacs
  :after (treemacs lsp-mode))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  :commands dap-mode
  :config (dap-mode 1)
  (require 'dap-ui)
  (dap-ui-mode 1))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package posframe)

(use-package ivy-posframe
  :after (ivy posframe))

(use-package plantuml-mode
  :mode "\\.puml\\'"
  :custom
  (plantuml-jar-path "/usr/local/bin/plantuml")
  (plantuml-default-exec-mode 'executable))

(use-package ag :ensure-system-package ag)

(use-package fzf :ensure-system-package fzf)

(use-package ivy
  :defer 0.1
  :diminish
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 15)
  (ivy-display-style 'fancy)
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
  ("C-x C-f" . counsel-find-file)
  ("C-c f" . fzf-find-file)
  ("C-c a" . counsel-ag)
  ("C-c g" . counsel-git)
  ("C-c l t" . counsel-load-theme)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-h o" . counsel-describe-symbol)
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package amx
  :defer 0.5
  :config (amx-mode))

(use-package editorconfig
  :config (editorconfig-mode 1))

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

(use-package all-the-icons)

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
  :config (evil-escape-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package yaml-mode)

(use-package ace-window)

(use-package terraform-mode)

(use-package haskell-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
          ("s-p" . projectile-command-map)
          ("C-c p" . projectile-command-map)))

(use-package origami
  :init (global-origami-mode))

(use-package lsp-origami
  :after (lsp origami)
  :hook ((lsp-after-open . lsp-origami-mode)))


(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; (defun apply-theme (appearance)
;;   "Load theme, taking current system APPEARANCE into consideration."
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (pcase appearance
;;     ('light (load-theme 'doom-tomorrow-day t))
;;     ('dark (load-theme 'doom-tomorrow-night t))))


(add-hook 'ns-system-appearance-change-functions #'apply-theme)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
    auto-package-update-interval 4))

(provide 'deps)
