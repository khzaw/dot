
;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:

;;; load path
(add-to-list 'load-path "~/.emacs.d/basics")

;;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file))


(setq package-check-signature nil)
(when (eq system-type 'darwin)
  (setq insert-directory-program "/usr/local/bin/gls"))
(setq dired-listing-switches "-aBhl --group-directories-first")

;;; gs-cons-threshold
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(setq read-process-output-max (* 1024 1024))

;;; package archives
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

(require 'looks)
(require 'layout)
(require 'bindings)
(require 'deps)
(require 'init-terminal)
(require 'init-window)
(require 'init-lsp)
(require 'init-go)
(require 'init-git)
(require 'init-org)
(require 'init-prog)
(require 'init-python)
(require 'init-rust)
(require 'init-js)
(require 'init-web)


(provide 'init)
;;; init.el ends here
