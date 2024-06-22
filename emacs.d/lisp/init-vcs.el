(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-auto-revert-mode t)
  :config
  (setq magit-save-repository-buffers 'dontask)
  (setq display-line-numbers-type 'visual)
  (setq magit-section-disable-line-numbers nil)
  ;; (customize-set-variable
  ;;   'display-buffer-alist
  ;;   '(("\\*magit: .*" display-buffer-same-window)))
  ;; Suppress the message
                                        ;
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  ;; (setq magit-bury-buffer-function #'quit-window) ;; let shackle handle this
  (setq magit-bury-buffer-function #'magit-mode-quit-window)
  (setq magit-refresh-status-buffer t))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode)
  :disabled t
  :config
  (add-to-list 'magit-delta-delta-args "--light" "--no-gitconfig"))

(use-package magit-todos
  :after magit)

(setq auth-sources (list
                    (concat (getenv "XDG_CONFIG_HOME") "/authinfo.gpg")
                    "~/.authinfo"
                    "~/.authinfo.gpg"))

(use-package forge
  :after magit
  :config
  ;; A topic is an issue or PR and the list of each can be configured
  ;; to display a number of open and closed items.
  ;; Show 100 open topics and never show any closed topics, for both
  ;; issues and PRs.
  (setq forge-topic-list-limit '(60 . 5)))

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
  :straight (:type git :host github :repo "artawower/blamer.el")
  :bind (("C-c g i" . blamer-show-commit-info)
         ("C-c g p" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-view 'overlay-right)
  (blamer-idle-time 0.3)
  (blamer-min-offset 70))

(use-package consult-gh
  :straight (:type git :host github :repo "armindarvish/consult-gh" :branch "develop")
  :after (consult forge transient)
  :config
  (require 'consult-gh-embark)
  (require 'consult-gh-forge)
  (require 'consult-gh-transient)
  (setq consult-gh-default-orgs-list '("khzaw" "projectrangoon" "algo-koans" "deliveryhero"))
  (setq consult-gh-default-clone-directory "~/Code")
  (setq consult-gh-show-preview t
        consult-gh-preview-key "M-.")
  (setq consult-gh-issue-action #'consult-gh--issue-view-action ;; view issues inside emacs
        consult-gh-repo-action #'consult-gh--repo-browse-files-action ;; browse files inside emacs
        consult-gh-file-action #'consult-gh--files-view-action) ;; open files in an emacs buffer
  (add-to-history 'savehist-additional-variables 'consult-gh--known-orgs-list) ;; keep record of searched orgs
  (add-to-history 'savehist-additional-variables 'consult-gh--known-repos-list)) ;; keep record of searched repos

(use-package consult-git-log-grep
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

(use-package ediff
  :straight (:type built-in)
  :hook
  ;; show org ediffs unfolded
  (edit-prepare-buffer . outline-show-all)
  ;; restore window layout when done
  (ediff-quit . winner-undo))

(use-package conventional-commit
  :straight (:host github :repo "akirak/conventional-commit.el")
  :hook
  (git-commit-mode . conventional-commit-setup))

(use-package git-gutter
  :bind (("C-x v C-g" . git-gutter-mode)
         ("C-x v p" . git-gutter:previous-hunk)
         ("C-x v n" . git-gutter:next-hunk)))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(provide 'init-vcs)
;;; init-vcs.el ends here
