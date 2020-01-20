(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(setq package-selected-packages '(evil
                                  evil-escape
                                  editorconfig
                                  magit
                                  smooth-scrolling
                                  dired-x
                                  whick-key
                                  org-bullets
                                  company-mode
                                  fzf
                                  one-themes
                                  doom-themes
                                  moe-theme
                                  cyberpunk-theme
                                  minimal-theme
                                  grandshell-theme
                                  poet-theme
                                  web-mode
                                  centered-window
                                  prettier-js
                                  protobuf-mode
                                  markdown-mode
                                  helm
                                  helm-projectile
                                  restart-emacs
                                  projectile
                                  fancy-battery
                                    ))
(package-initialize)
(setq package-enable-at-startup nil)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load-theme 'doom-molokai t)

;; Load mail config file ~/.config.org
(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(provide 'init)
