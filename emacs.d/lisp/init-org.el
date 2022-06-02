(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  (require 'org-tempo)
  :bind
  ("C-c C-c" . org-edit-src-exit)
  :custom
  (org-pretty-entities t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)        ; use native major-mode indentation
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'other-window)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-hide-leading-starts t)
  (org-startup-truncated nil)
  (org-imenu-depth 6)
  (org-tags-column 0)
  (org-startup-indented t)
  (org-startup-folded nil)
  (org-special-ctrl-a/e t)
  (org-M-RET-may-split-line nil)
  (org-insert-heading-respect-content t) ; insert new headings after current subtree rather than inside it
  (org-priority-faces
    '((?A . error)
       (?B . warning)
       (?C . success)))
  (org-confirm-babel-evaluate nil)
  (org-link-elisp-confirm-function nil))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode)))

(use-package org-bullets
  :if (display-graphic-p)
  :after org
  :hook (org-mode . org-bullets-mode))

(provide 'init-org)
;;; init-org.el ends here
