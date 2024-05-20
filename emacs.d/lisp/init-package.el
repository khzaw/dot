
;; setup straight.el
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))
(setq straight-use-package-by-default t)

;; (require 'package)
;; Initialize packages


;; (package-initialize)

;; Setup `use-package'
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; ;; Should set before loading `use-package'
;; (eval-and-compile
;;   (setq use-package-always-ensure t)
;;   (setq use-package-expand-minimally t)
;;   (setq use-package-enable-imenu-support t))

;; (eval-when-compile
;;   (require 'use-package))


;; `require-with-check' is a very new function
;; projectile  loads project but doesn't declare it as a dependency
;; and straight.el does not know that it needs to `project and the built-in version gets loaded.
;; But then eglot declare `project' as a dependency which causes striaght.el to install project.
;; But ther other version was already loaded, so `require-with-check correctly identifies that
;; there is a confusing situation.
;; So load `project before everything else.'
(use-package project)

;; Required by `use-package'
(use-package diminish)

(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; Auto update packages
(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t))

;; profiler
(use-package esup)

;; Garbage Collector Magic Hack
(use-package gcmh
  :init
  (setq gcmh-idle-delay 5)
  (setq gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-mode))

(provide 'init-package)
;;; init-package.el ends here
