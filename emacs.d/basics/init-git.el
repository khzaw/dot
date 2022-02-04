;;; package --- git.el
;;; Commentary

(use-package magit
  :init
  (setq magit-save-some-buffers nil) ; don't ask to save buffers
  :bind ("C-x g" . magit-status))

(use-package git-timemachine)

(provide 'init-git)
