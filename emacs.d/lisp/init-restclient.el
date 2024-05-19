(use-package restclient
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
     ("image/jpg" . image-mode))))

(provide 'init-restclient)
