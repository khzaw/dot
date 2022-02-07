;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/basics")

(setq gs-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(when (eq system-type 'darwin)
  (setq insert-directory-program "/usr/local/bin/gls"))
(setq dired-listing-switches "-aBhl --group-directories-first")

;;; custom files
(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file))

(require 'defaults)
(require 'layout)
(require 'bindings)


(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages")))
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package use-package-ensure-system-package)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
  '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(require 'deps)
(require 'init-window)
(require 'init-git)
(require 'init-org)
(require 'init-prog)
(require 'init-python)
(require 'init-go)
(require 'init-rust)
(require 'init-web)

(provide 'init)
;;; init.el ends here
