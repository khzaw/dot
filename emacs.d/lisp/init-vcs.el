(use-package magit
  :config (setq magit-save-repository-buffers t)
  :bind ("C-x g" . magit-status))

(use-package magit-todos :after magit)

(use-package forge :after magit)

(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine-toggle))

(use-package git-messenger
  :bind ("C-c g m" . git-messenger:popup-message)
  :init (setq git-messenger:show-detail t
          git-messenger:use-magit-popup t))
;; :config
;; (progn
;;   (define-key git-messenger-map (kbd "RET") 'git-messenger:popup-close)))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :bind (:map vc-prefix-map
          ("B" . browse-at-remote)))

(use-package git-modes)

(use-package gitignore-templates)

(provide 'init-vcs)
;;; init-vcs.el ends here
