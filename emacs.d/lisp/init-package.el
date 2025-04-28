;;; -*- lexical-binding: t; -*-

;; setup straight.el
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))
(setq straight-use-package-by-default t)
;; https://github.com/radian-software/straight.el/issues/1146
(setq straight-built-in-pseudo-packages
  (append straight-built-in-pseudo-packages '(project xref)))
(straight-use-package 'org)


;; Required by `use-package'
(use-package diminish)

(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)
;; profiler

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :init
  (setq gcmh-idle-delay 5)
  (setq gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-mode))

(use-package epkg
  :hook (epkg-list-mode . (lambda () (setq truncate-lines t)))
  :bind (:map epkg-list-mode
         ("j" . next-line)
         ("k" . previous-line)
         ("u" . beginning-of-buffer)
         ("q" . kill-buffer-and-window)))

(provide 'init-package)
;;; init-package.el ends here
