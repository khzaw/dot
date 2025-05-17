;; -*- lexical-binding: t; -*-
(use-package docker
  :defer t
  :bind ("C-c D" . docker)
  :config (setq docker-containers-show-all t))

(use-package dockerfile-mode)

(use-package docker-compose-mode
  :mode ("docker-compose.*\.yml\\'" . docker-compose-mode))

(provide 'init-docker)
;;; init-docker.el ends here
