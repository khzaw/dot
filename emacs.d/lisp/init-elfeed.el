;; (setq shr-max-width fill-column)
(setq shr-use-fonts nil)

(use-package elfeed
  :bind ("C-c z w" . elfeed)
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (setq elfeed-show-entry-switch 'display-buffer)
  (defun elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))
  (define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list  (expand-file-name "elfeed.org" user-emacs-directory))))

(provide 'init-elfeed)
