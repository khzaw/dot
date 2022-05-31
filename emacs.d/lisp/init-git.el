(setq vc-follow-symlinks t
      find-file-visit-truename t)

(use-package magit
  :config
  (setq magit-save-repository-buffers t)
  :bind ("C-x g" . magit-status))

(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine-toggle))

(use-package git-messenger
  :bind ("C-c g m" . git-messenger:popup-message)
  :init (setq git-messenger:show-detail t
              git-messenger:use-magit-popup t)
  :config
  (progn
    (define-key git-messenger-map (kbd "RET") 'git-messenger:popup-close)))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :bind (:map vc-prefix-map
         ("B" . browse-at-remote)))

(provide 'init-git)
;;; init-git.el ends here
