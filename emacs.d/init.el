;;; package --- Summary
;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:

(defun config-reload ()
  "Uncle dev created a function to reload Emacs config."
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

(require 'init-layout)
(require 'init-bindings)

(require 'init-basics)
(require 'init-font)
(require 'init-ui)
(require 'init-modeline)
(require 'init-themes)
(require 'init-dashboard)

(require 'init-evil)
(require 'init-edit)
(require 'init-highlight)
(require 'init-consult)
;; (require 'init-company)
(require 'init-corfu)
(require 'init-dired)
(require 'init-window)
(require 'init-treemacs)

;; tools
(require 'init-compile)
(require 'init-treesitter)
(require 'init-docker)
(require 'init-projectile)
(require 'init-flymake)
;; (require 'init-flycheck)
(require 'init-eglot)
(require 'init-lsp)
(require 'init-vcs)
(require 'init-terminal)

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
(require 'init-prog)

(require 'init-org)
;; Others
(require 'init-presentation)
(require 'init-elfeed)
(require 'init-spell)
(require 'init-others)

(provide 'init)
;;; init.el ends here
