
(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  (require 'org-tempo)
  (setq org-src-fontify-natively t)
  (setq org-pretty-entities t)
  (setq org-src-preserve-indentation t)
  (setq org-confirm-babel-evaluate t)
  (setq org-src-fontify-natively t))

(use-package company-org-block
  :custom
  (company-org-block-edit-styel 'auto) ;; 'auto, 'prompt, 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(provide 'org)