;;; package --- Summary
;;; Commentary:
;;; Code:
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

(use-package org-bullets
  :if (display-graphic-p)
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-dir (concat (getenv "HOME") "/Dropbox/org/journal")))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
    (("s-Y" . org-download-screenshot)
     ("s-y" . org-download-yank))))

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory (file-truename (concat (getenv "HOME") "/Dropbox/org/notes")))
  :config
  (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                 "#+TITLE: ${title}\n#+DATE: %T\n")
                                       :unnarrowed t)))
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         (:map org-mode-map (("C-c n i" . org-roam-node-insert)
                             ("C-c n o" . org-id-get-create)
                             ("C-c n t" . org-roam-tag-add)
                             ("C-c n a" . org-roam-alias-add)
                             ("C-c n l" . org-roam-buffer-toggle)))))

(use-package company-org-block
  :custom
  (company-org-block-edit-styel 'auto) ;; 'auto, 'prompt, 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(provide 'init-org)
;;; init-org.el ends here
