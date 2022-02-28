;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:
(package-initialize)

(setq gs-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;;; custom files
(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file))

(when (eq system-type 'darwin)
  (setq insert-directory-program "/usr/local/bin/gls"))
(setq dired-listing-switches "-aBhl --group-directories-first")

(add-to-list 'load-path "~/.emacs.d/basics")
(require 'defaults)
(require 'layout)
(require 'bindings)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package-archive-contents
  (package-refresh-contents))
(package-install 'use-package)
(use-package use-package-ensure
  :config (setq use-package-always-ensure t))

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


(require 'deps)
(require 'init-window)
(require 'init-git)
(require 'init-org)
(require 'init-prog)
(require 'init-python)
(require 'init-go)
(require 'init-rust)
(require 'init-js)
(require 'init-web)

(provide 'init)
;;; init.el ends here
