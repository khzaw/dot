;;; package --- git.el
;;; Commentary
;;; Code:

(use-package magit
  :config
  (setq magit-save-repository-buffers t)
  :bind ("C-x g" . magit-status))

;; diff-hl shows uncommitted git changes on left side of the buffer.
(use-package diff-hl
  :defer 1
  :hook
  (dired-mode . diff-hl-dired-mode-unless-remote)
  :config
  (global-diff-hl-mode 1))

(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine-toggle))

(use-package git-messenger
  :bind ("C-c g m" . git-messenger:popup-message)
  :init (setq git-messenger:show-detail t)
  :config
  (progn
    (define-key git-messenger-map (kbd "RET") 'git-messenger:popcup-close)))

(provide 'init-git)
;;; init-git.el ends here
