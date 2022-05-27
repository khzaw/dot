
;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:

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
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;;; load path
(add-to-list 'load-path (expand-file-name "basics" user-emacs-directory))

(require 'use-package)
(setq use-package-always-ensure t)
;; (setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

(require 'looks)
(require 'layout)
(require 'bindings)
(require 'deps)
(require 'init-window)
(require 'init-git)
(require 'init-org)
(require 'init-prog)
(require 'init-python)
(require 'init-go)
(require 'init-rust)
(require 'init-lsp)
(require 'init-js)
(require 'init-web)
(require 'init-terminal)

(provide 'init)

;; bootstrap quelpa
;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))
;; ;; then bootstrap quelpa-use-package which will pull in use-package
;; (quelpa
;;   '(quelpa-use-package
;;      :fetcher git
;;      :url "https://github.com/quelpa/quelpa-use-package.git"))
;; (require 'quelpa-use-package)
;; (setq use-package-ensure-function 'quelpa)
;; (setq use-package-always-ensure t)


;;; init.el ends here
