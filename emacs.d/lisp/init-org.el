(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  (require 'org-tempo)
  :bind
  ("C-c C-c" . org-edit-src-exit)
  :custom
  (org-directory (concat (getenv "HOME") "/Dropbox/notes/"))
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

(use-package org-superstar
  :if (display-graphic-p)
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename org-directory))
  :bind (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n f" . org-roam-node-find)
          ("C-c n g" . org-roam-graph)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n c" . org-roam-capture)
          ;; Dailies
          ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
    '(("d" "default" entry
        "* %?"
        :target (file+head "%<%Y-%m-%d>.org"
                  "#+title: %<%Y-%m-%d>\n"))))
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (require 'org-roam-export))

(use-package deft
  :config
  (setq deft-directory (file-truename org-directory)
    deft-recursive t
    deft-strip-summary-regexp  ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
    deft-use-filename-as-title t)
  :bind
  ("C-c n d" . deft))


(provide 'init-org)
;;; init-org.el ends here
