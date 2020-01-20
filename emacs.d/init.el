(add-to-list 'default-frame-alist '
  (ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '
  (ns-appearance . dark))

(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors 
  (load custom-file))

(require 'package)
(setq package-archives '
  (
    ("org"       . "https://orgmode.org/elpa/")
    ("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa"     . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(package-initialize)

(unless 
  (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(require 'org)
(org-babel-load-file 
  (expand-file-name "~/.emacs.d/config.org"))
