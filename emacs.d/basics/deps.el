;;; package --- deps.el
;;; Commentary:

;;; Code:
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

;; Show native line numbers if possible, otherwise use `linum`
(if (fboundp 'display-line-numbers-mode)
  (use-package display-line-numbers
    :ensure nil
    :hook (yaml-mode . display-line-numbers-mode)
    :init (setq display-line-numbers-width-start t))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :init (setq linum-format "%4d")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))


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

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

(use-package solidity-mode
  :config
  (setq solidity-comment-style 'slash)
  (setq solidity-solc-path "/usr/local/bin/solc")
  :bind
  (:map solidity-mode-map
    ("C-c s g" . solidity-estimate-gas-at-point)))

(use-package solidity-flycheck
  :after (solidity-mode flycheck))

(use-package company-solidity
  :after (solidity-mode company))

;; Save command history on disk, so the sorting gets more intelligent over time
(use-package prescient
  :defer 1
  :config
  (prescient-persist-mode 1))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
          ([remap completion-at-point] . company-complete)
          ("<tab>" . company-complete-selection)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous))
        (:map lsp-mode-map
              ("<tab>" . company-indent-or-complete-common))
  :config
  (progn
    (setq company-tooltip-align-annotations t
          company-show-numbers t))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (global-company-mode t))

(use-package company-prescient
  :defer 1
  :after (company prescient)
  :config
  (company-prescient-mode 1))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-posframe
  :after (company posframe))

(use-package company-quickhelp
  :after company
  :config (company-quickhelp-mode 1))

(use-package treemacs
  :defer t
  :config
  (progn
    (setq
      treemacs-eldoc-display t
      treemacs-silent-refresh t
      treemacs-silent-filewatch t
      treemacs-show-hidden-files t
      treemacs-is-never-other-window t
      treemacs-user-mode-line-format 'none
      treemacs-is-never-other-window t))
  :bind
  (:map global-map
    ("C-c t t" . treemacs)
    ("C-c t d" . treemacs-select-directory)
    ("C-c t C-f" . treemacs-find-file)
    ("C-c t 1" . treemacs-delete-other-windows)
    ("C-c t 0" . treemacs-select-window)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons))

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :after (treemacs dired)
  :config
  (treemacs-icons-dired-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
          (lsp-after-open . lsp-origami-try-enable))
  :commands (lsp lsp-deferred)
  :config
  (setq
    lsp-eldoc-hook nil
    lsp-headerline-breadcrumb-enable nil
    lsp-enable-imenu nil
    lsp-idle-delay 0.5))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-doc-show-with-cursor t))

(use-package lsp-ivy
  :after (lsp-mode ivy))

(use-package lsp-treemacs
  :after (treemacs lsp-mode))

(use-package yasnippet
  :hook ((lsp-mode . yas-minor-mode))
  :config (yas-global-mode 1))

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  :commands dap-mode
  :config (dap-mode 1)
  (require 'dap-ui)
  (dap-ui-mode 1))

(use-package format-all
  :bind (:map prog-mode-map ("M-<f8>" . format-all-buffer)))

(use-package flycheck
 :defer 1
 :init (global-flycheck-mode))

(use-package flycheck-posframe
 :defer 1
 :after (flycheck posframe)
 :hook (flycheck-mode . flycheck-posframe-mode)
 :config
 (flycheck-posframe-configure-pretty-defaults)
 (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p)
 (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)
 (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p))

(use-package posframe)

(use-package ivy-posframe
  :after (ivy posframe)
  :config
  (setq ivy-posframe-parameters ''(internal-border-width . 10)))

(use-package plantuml-mode
  :mode "\\.puml\\'"
  :custom
  (plantuml-jar-path "/usr/local/bin/plantuml")
  (plantuml-default-exec-mode 'executable))

(use-package ag :defer 0.1)

(use-package fzf :defer 0.1)

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
          ("C-c C-r" . ivy-resume)))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x b" . counsel-switch-buffer)
  ("C-x C-f" . counsel-find-file)
  ("C-c f" . fzf-find-file)
  ("C-c a" . counsel-ag)
  ;; ("C-c f" . counsel-git)
  ("C-c i" . counsel-imenu)
  ("C-c l t" . counsel-load-theme)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-h o" . counsel-describe-symbol)
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package counsel-spotify
  :after counsel
  :init
  (setq counsel-spotify-client-id 'spotify-client-id
    counsel-spotify-client-secret 'spotify-secret)
  :bind
  (("C-c C-s p" . counsel-spotify-toggle-play-pause)
  ("C-c C-s n" . counsel-spotify-next)
  ("C-c C-s r" . counsel-spotify-prev)
  ("C-c C-s s" . counsel-spotify-search-track)
  ("C-c C-s a" . counsel-spotify-search-artist)
  ("C-c C-s l" . counsel-spotify-search-playlist)))

(use-package amx
  :defer 0.5
  :config (amx-mode))

(use-package editorconfig
  :defer 1
  :config (editorconfig-mode 1))

(use-package undo-tree
  :diminish
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/Code/dot/emacs.d/undo-tree")))
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

(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-char-position 'left
              aw-ignore-current nil
              aw-leading-char-style 'char
              aw-scope 'frame)
  :bind (("M-o" . ace-window)
          ("M-O" . ace-swap-window)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package hydra)

(use-package evil
  :ensure t
  :init
  (progn
    (setq evil-want-keybinding nil)
    (setq evil-split-window-below t)
    (setq evil-vsplit-window-right t))
  :config
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'fundamental-mode 'emacs)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
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

(use-package yaml-mode)

(use-package terraform-mode)

(use-package haskell-mode :defer t)

(use-package protobuf-mode :defer t)

(use-package restclient
  :mode ("\\.restclient$" . restclient-mode))

;; Curly quotes when writing in markup languages
(use-package typo :defer t)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . gfm-mode)
          ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package projectile
  :init
  (projectile-mode +1)
  :config (setq projectile-enable-caching t
            projectile-completion-system 'ivy)
  :bind (:map projectile-mode-map
          ("C-c p" . projectile-command-map)))

(use-package origami
  :init (global-origami-mode))

;; (use-package lsp-origami
;;   :after (lsp origami)
;;   :hook ((lsp-after-open . lsp-origami-try-enable)))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-moonlight t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package kaolin-themes
  :config
  (kaolin-treemacs-theme))

(use-package solaire-mode
  :defer 1
  :hook
  ;; Ensure solaire-mode is running in all solaire-mode buffers
  (change-major-mode . turn-on-solaire-mode)
  (after-revert . turn-on-solaire-mode)
  (ediff-prepare-buffer . solaire-mode)
  :custom
  (solaire-mode-auto-swap-bg t)
  :config
  (solaire-global-mode +1))

(use-package prescient
  :defer 1
  :config
  (prescient-persist-mode 1))


;; (defun apply-theme (appearance)
;;   "Load theme, taking current system APPEARANCE into consideration."
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (pcase appearance
;;     ('light (load-theme 'doom-tomorrow-day t))
;;     ('dark (load-theme 'doom-tomorrow-night t))))
;; (add-hook 'ns-system-appearance-change-functions #'apply-theme)

(use-package writeroom-mode
  :bind (:map writeroom-mode-map
          ("C-c C-w ,"   . #'writeroom-decrease-width)
          ("C-c C-w ."   . #'writeroom-increase-width)
          ("C-c C-w /"   . #'writeroom-adjust-width)
          ("C-c C-w SPC" . #'writeroom-toggle-mode-line))
  (:map global-map
    ("C-c C-M-w" . #'writeroom-mode)))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
    auto-package-update-interval 4))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode
  :mode ("docker-compose.*\.yml\\'" . docker-compose-mode))

(use-package shell-pop
  :bind ("s-t" . shell-pop))

(use-package golden-ratio
  :config
  (setq golden-ratio-auto-scale 1))

(use-package smart-tab
  :init
  (progn
    (setq hippie-expand-try-functions-list '(yas-hippie-try-expand
                                             try-complete-file-name-partially))
                                        ;try-expand-dabbrev
                                        ;try-expand-dabbrev-visible
                                        ;try-expand-dabbrev-all-buffers
                                        ;try-complete-lisp-symbol-partially
                                        ;try-complete-lisp-symbol
    (setq smart-tab-debug t)
    (setq smart-tab-user-provided-completion-function 'company-complete)
    (setq smart-tab-using-hippie-expand t)
    (setq smart-tab-disabled-major-modes '(org-mode term-mode eshell-mode inferior-python-mode))
    (global-smart-tab-mode 1)))

(use-package presentation
  :config
  (global-set-key (kbd "<M-f5>") (lambda ()
                                   (interactive)
                                   (if presentation-mode (presentation-mode 0)
                                     (presentation-mode 1)))))

(provide 'deps)
;;; deps.el ends here
