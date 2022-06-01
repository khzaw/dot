(use-package docker
  :bind ("C-c D" . docker))

(use-package dockerfile-mode)

(use-package docker-compose-mode
  :mode ("docker-compose.*\.yml\\'" . docker-compose-mode))

(provide 'init-docker)
;;; init-docker.el ends here
