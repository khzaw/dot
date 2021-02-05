;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:
(setq gs-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 150
                    :weight 'normal
                    :width 'normal)
(setq-default line-spacing 3)
(set-window-margins (selected-window) 5 5)
;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file))

(require 'package)
(setq package-archives '(("melpa"     . "https://melpa.org/packages/")
                         ("org"       . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(provide 'init)
;;; init.el ends here
