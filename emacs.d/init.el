;;; -*- lexical-binding: t -*-
;;; package --- Summary
;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:

(defun config-reload ()
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

;; Always load newest byte code
(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 16777216 ; 16 mb
                  gc-cons-percentage 0.1)))

(defun doom-defer-garbage-collection-h ()
  "Disable garbage collection."
  (setq gs-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  "Restore garbage collection."
  (run-at-time
    1 nil (lambda () (setq gs-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)

(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

;; custom-file
(setq custom-file (locate-user-emacs-file "custom.el"))
;; (ignore-errors (load custom-file))

(setq package-check-signature nil)

;; (when (eq system-type 'darwin)
;;   (setq insert-directory-program "/usr/local/bin/gls"))
;; (setq dired-listing-switches "-aBhl --group-directories-first")

(require 'secrets)
(require 'init-package)
(require 'init-exec-path)
(require 'init-maintenance)

(require 'init-layout)
(require 'init-bindings)

(require 'init-basics)
(require 'init-font)
(require 'init-themes)
(require 'init-ui)
(require 'init-modeline)
;; (require 'init-dashboard)

(require 'init-evil)
(require 'init-edit)
(require 'init-highlight)
(require 'init-consult)
;; (require 'init-company)
(require 'init-corfu)
(require 'init-dired)
(require 'init-window)
(require 'init-treemacs)
(require 'init-helm)

;; tools
(require 'init-compile)
(require 'init-treesitter)
(require 'init-docker)
(require 'init-projectile)
(require 'init-flymake)
(require 'init-flycheck)
(require 'init-eglot)
(require 'init-dape)
(require 'init-lsp)
(require 'init-vcs)
(require 'init-terminal)

(require 'init-pragmatapro)

;; Programming languages
(require 'init-elisp)
(require 'init-java)
(require 'init-go)
(require 'init-haskell)
(require 'init-markdown)
(require 'init-ocaml)
(require 'init-python)
(require 'init-rust)
(require 'init-solidity)
(require 'init-tex)
(require 'init-web)
(require 'init-yaml)
(require 'init-kubernetes)
(require 'init-latex)
(require 'init-prog)

;; programming tools
(require 'init-restclient)

;; Others
(require 'init-org)
(require 'init-elfeed)
(require 'init-spell)
(require 'init-others)
(require 'init-presentation)
(require 'init-erc)
(require 'init-ai)
;; (require 'init-jira)
;; (require 'init-slack)

(provide 'init)
;;; init.el ends here
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
