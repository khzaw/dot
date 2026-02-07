;; -*- lexical-binding: t; -*-
;; (setq shr-max-width fill-column)
(setq shr-use-fonts nil)

(use-package elfeed
  :commands elfeed
  :bind
  ("C-c z w" . elfeed)
  (:map elfeed-search-mode-map
        ("R" . elfeed-update)
        ("w" . elfeed-search-yank)
        ("Q" . elfeed-kill-buffer)
        ("A" . (lambda () (interactive) ;; mark all as read
                 (mark-whole-buffer)
                 (elfeed-search-untag-all-unread))))
  (:map elfeed-show-mode-map
        ("S" . elfeed-show-new-live-search)
        ("c" . (lambda () (interactive) (org-capture nil "capture")))
        ("w" . elfeed-show-yank))
  :custom
  (elfeed-search-remain-on-entry t)
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width 30)
  (elfeed-search-trailing-width 25)
  (elfeed-show-truncate-long-urls t)
  (elfeed-sort-order 'descending)
  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer))

(use-package elfeed-org
  :custom
  (rmh-elfeed-org-files (list (expand-file-name "elfeed.org" user-emacs-directory)))
  :config
  (elfeed-org))

(use-package elfeed-goodies
  :config (elfeed-goodies/setup))

(use-package elfeed-webkit
  :bind (:map elfeed-show-mode-map
              ("%" . elfeed-webkit-toggle))
  :config (elfeed-webkit-auto-toggle-by-tag))

(provide 'init-elfeed)
