(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-auto-revert-mode t)
  :config
  (setq magit-save-repository-buffers 'dontask)
  (customize-set-variable
    'display-buffer-alist
    '(("\\*magit: .*" display-buffer-same-window))))

(use-package magit-todos :after magit)

(setq auth-sources (list
                     (concat (getenv "XDG_CONFIG_HOME") "/authinfo.gpg")
                     "~/.authinfo.gpg"))

(use-package forge
  :config
  ;; A topic is an issue or PR and the list of each can be configured
  ;; to display a number of open and closed items.
  ;; Show 100 open topics and never show any closed topics, for both
  ;; issues and PRs.
  (setq forge-topic-list-limit '(100 . 0))
  ;; (setq forge-topic-list-limit '(100 . -10))
  )


;; (use-package forge :after magit)

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

(use-package git-gutter
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package blamer
  :bind ("C-c g b" . blamer-show-posframe-commit-info)
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                  :height 140))))

(provide 'init-vcs)
;;; init-vcs.el ends here
