;; (setq shr-max-width fill-column)
(setq shr-use-fonts nil)

(use-package elfeed
  :bind ("C-c z w" . elfeed)
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (setq elfeed-show-entry-switch 'display-buffer))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "elfeed.org")))

(provide 'init-elfeed)
