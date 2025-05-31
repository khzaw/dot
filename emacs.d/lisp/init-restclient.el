;; -*- lexical-binding: t; -*-

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.restclient$" . restclient-mode))
  :commands (restclient-jump-next restclient-jump-prev)
  :custom
  (restclient-same-buffer-response-name "*restclient*")
  (restclient-content-type-modes
   '(("application/json" . json-mode)
     ("text/yaml" . yaml-mode)
     ("application/yaml" . yaml-mode)
     ("application/x-yaml" . yaml-mode)
     ("image/gif" . image-mode)
     ("image/png" . image-mode)
     ("image/jpeg" . image-mode)
     ("image/jpg" . image-mode)))
  :config
  (use-package restclient-test
    :diminish
    :hook (restclient-mode . restclient-test-mode)))

(use-package verb)

(use-package grpclient
  :if (executable-find "grpcurl")
  :straight (:type git :host github :repo "Prikaz98/grpclient.el")
  :init (add-to-list 'auto-mode-alist '("\\.grpc\\'" . grpclient-mode)))

(use-package openapi-preview
  :if (executable-find "redoc-cli")
  :straight (:type git :host github :repo "merrickluo/openapi-preview"))

(use-package impostman
  :straight (:type git :host github :repo "flashcode/impostman"))

(provide 'init-restclient)
