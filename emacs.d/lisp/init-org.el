(use-package org
  :straight nil
  :custom
  (org-pretty-entities t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'other-window)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-confirm-babel-evaluate t))

(use-package org-contrib)

;; (use-package org
;;   ;; :straight (:files (:defaults "lisp/*"))
;;   :straight (:type built-in)
;;   :hook (org-mode . org-indent-mode)
;;   :bind
;;   ("C-c C-c" . org-edit-src-exit)
;;   :custom
;;   (org-directory (concat (getenv "HOME") "/Dropbox/notes/"))
;;   ;; (org-agenda-files '("gsd.org"  "work.org"))
;;   (org-todo-keywords '((sequence "TODO(t)" "DOING(n)" "|" "DONE(d)")))
;;   ;; (org-agenda-start-with-log-mode t)
;;   ;; (org-log-done 'time)
;;   ;; (org-log-into-drawer t)
;;   (org-pretty-entities t)
;;   (org-src-fontify-natively t)
;;   (org-src-preserve-indentation t)        ; use native major-mode indentation
;;   (org-src-tab-acts-natively t)
;;   (org-src-window-setup 'other-window)
;;   (org-fontify-done-headline t)
;;   (org-fontify-quote-and-verse-blocks t)
;;   (org-fontify-whole-heading-line t)
;;   (org-hide-leading-stars t)
;;   (org-hide-emphasis-markers t)
;;   (org-startup-truncated nil)
;;   (org-imenu-depth 6)
;;   (org-tags-column 0)
;;   (org-startup-indented t)
;;   (org-startup-folded nil)
;;   (org-special-ctrl-a/e t)
;;   (org-link-search-must-match-exact-headline nil)
;;   (org-M-RET-may-split-line nil)
;;   (org-insert-heading-respect-content t) ; insert new headings after current subtree rather than inside it
;;   (org-priority-faces
;;     '((?A . error)
;;        (?B . warning)
;;        (?C . success)))
;;   (org-confirm-babel-evaluate nil)
;;   (org-link-elisp-confirm-function nil)
;;   :config
;;   (require 'org-tempo)

;;   (use-package org-modern
;;     :hook ((org-mode . org-modern-mode)))

;;   (use-package ox-gfm :after org)
;;   (add-to-list 'org-export-backends 'md)

;;   (defconst load-language-alist
;;     '((emacs-lisp . t)
;;        (perl . t)
;;        (python . t)
;;        (ruby . t)
;;        (js . t)
;;        (css . t)
;;        (sass . t)
;;        (plantuml . t))
;;     "Alist of org ob languages.")

;;   ;; ob-sh renamed to ob-shell since 26.1.
;;   (cl-pushnew '(shell . t) load-language-alist)

;;   (use-package ob-go
;;     :if (executable-find "go")
;;     :init (cl-pushnew '(go . t) load-language-alist))

;;   (use-package ob-rust
;;     :if (executable-find "rustc")
;;     :init (cl-pushnew '(rust . t) load-language-alist))

;;   ;; npm install -g @mermaid-js/mermaid-cli
;;   ;; (use-package ob-mermaid
;;   ;;   :init (cl-pushnew '(mermaid . t) load-language-alist))
;;   (org-babel-do-load-languages 'org-babel-load-languages load-language-alist)

;;   (use-package org-rich-yank
;;     :bind (:map org-mode-map
;;             ("C-M-y" . org-rich-yank)))

;;   (use-package org-preview-html
;;     :diminish
;;     :bind (:map org-mode-map
;;             ("C-c C-h" . org-preview-html-mode))
;;     :init (when (featurep 'xwidget-internal)
;;             (setq org-preview-html-viewer 'xwidget)))

;;   (use-package org-roam
;;     :diminish
;;     :custom
;;     (org-roam-directory (file-truename org-directory))
;;     (org-roam-capture-templates
;;       '(
;;          ("d" "default" plain
;;            "%?"
;;            :if-new (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org" "#+title: ${title}\n")
;;            :unnarrowed t)
;;          ("b" "book notes" plain
;;            "\n* Source \n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
;;            :if-new (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org" "#+title: ${title}\n")
;;            :unnarrowed t)))
;;     :bind (("C-c n l" . org-roam-buffer-toggle)
;;             ("C-c n f" . org-roam-node-find)
;;             ("C-c n g" . org-roam-graph)
;;             ("C-c n i" . org-roam-node-insert)
;;             ("C-c n c" . org-roam-capture)
;;             ;; Dailies
;;             ("C-c n j" . org-roam-dailies-capture-today))
;;     :config
;;     (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;     (org-roam-db-autosync-mode)
;;     (setq org-roam-dailies-directory "daily/")

;;     ;; If using org-roam-protocol
;;     (require 'org-roam-protocol)
;;     (require 'org-roam-export))

;;   (use-package consult-org-roam :after (consult org-roam)))


;; (use-package evil-org
;;   :after org
;;   :hook (org-mode . (lambda () evil-org-mode))
;;   :config
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))


;; (use-package deft
;;   :config
;;   (setq deft-directory (file-truename org-directory)
;;     deft-recursive t
;;     deft-strip-summary-regexp  ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
;;     deft-use-filename-as-title t
;;     deft-auto-save-interval 0)
;;   :bind
;;   ("C-c n d" . deft))

(provide 'init-org)
;;; init-org.el ends here
