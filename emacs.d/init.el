;;; package --- Summary
;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:

;; Always load newest byte code
(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; gs-cons-threshold
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
  (lambda ()
    "Recover GC values after startup."
    (setq gc-cons-threshold 800000
      gc-cons-percentage 0.1)))

;; custom-file
(setq custom-file (locate-user-emacs-file "custom.el"))
(ignore-errors (load custom-file))

;; (setq package-check-signature nil)
;; (when (eq system-type 'darwin)
;;   (setq insert-directory-program "/usr/local/bin/gls"))
;; (setq dired-listing-switches "-aBhl --group-directories-first")

(require 'secrets)
(require 'init-package)
(require 'init-exec-path)

(require 'init-layout)
(require 'init-bindings)

(require 'init-basics)

(require 'init-ui)
(require 'init-evil)
(require 'init-edit)
(require 'init-highlight)
(require 'init-consult)
(require 'init-ivy)
(require 'init-company)

(require 'init-dashboard)
(require 'init-dired)
(require 'init-window)
(require 'init-treemacs)

(require 'init-org)

;; Programming
(require 'init-docker)
(require 'init-flycheck)
(require 'init-vcs)
(require 'init-lsp)
(require 'init-projectile)

(require 'init-elisp)
(require 'init-go)
(require 'init-haskell)
(require 'init-markdown)
(require 'init-python)
(require 'init-rust)
(require 'init-solidity)
(require 'init-tex)
(require 'init-web)
(require 'init-prog)

(require 'init-terminal)

;; Others
(require 'init-presentation)

(provide 'init)
;;; init.el ends here
