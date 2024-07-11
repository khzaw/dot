
(use-package helm)

(use-package helm-describe-modes
  :bind ([remap describe-mode] . helm-describe-modes))

(provide 'init-helm)
