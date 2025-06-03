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

(use-package org
  :defer
  :straight `(org
              :fork (:host nil
                     :repo "https://git.tecosaur.net/tec/org-mode.git"
                     :branch "dev"
                     :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
               (require 'lisp-mnt)
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents "lisp/org.el")
                        (lm-header "version")))
                     (git-version
                      (string-trim
                       (with-temp-buffer
                         (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                         (buffer-string)))))
                (insert
                 (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                 (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                 "(provide 'org-version)\n")))
              :pin nil))


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
