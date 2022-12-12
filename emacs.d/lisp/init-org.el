(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t))

(use-package org
  :init
  :hook (org-mode . org-indent-mode)
  :bind
  ("C-c C-c" . org-edit-src-exit)
  :custom
  (org-directory (concat (getenv "HOME") "/Dropbox/notes/"))
  ;; (org-agenda-files '("gsd.org"  "work.org"))
  (org-todo-keywords '((sequence "TODO(t)" "DOING(n)" "|" "DONE(d)")))
  ;; (org-agenda-start-with-log-mode t)
  ;; (org-log-done 'time)
  ;; (org-log-into-drawer t)
  (org-pretty-entities t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)        ; use native major-mode indentation
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'other-window)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-startup-truncated nil)
  (org-imenu-depth 6)
  (org-tags-column 0)
  (org-startup-indented t)
  (org-startup-folded nil)
  (org-special-ctrl-a/e t)
  (org-link-search-must-match-exact-headline nil)
  (org-M-RET-may-split-line nil)
  (org-insert-heading-respect-content t) ; insert new headings after current subtree rather than inside it
  (org-priority-faces
    '((?A . error)
       (?B . warning)
       (?C . success)))
  (org-confirm-babel-evaluate nil)
  (org-link-elisp-confirm-function nil)
  :config

  (require 'org-tempo)

  (use-package org-modern
    :hook ((org-mode . org-modern-mode)))

  (use-package ox-gfm :after org)
  (add-to-list 'org-export-backends 'md)

  (defconst load-language-alist
    '((emacs-lisp . t)
       (perl . t)
       (python . t)
       (ruby . t)
       (js . t)
       (css . t)
       (sass . t)
       (plantuml . t))
    "Alist of org ob languages.")

  ;; ob-sh renamed to ob-shell since 26.1.
  (cl-pushnew '(shell . t) load-language-alist)

  (use-package ob-go
    :if (executable-find "go")
    :init (cl-pushnew '(go . t) load-language-alist))

  (use-package ob-rust
    :if (executable-find "rustc")
    :init (cl-pushnew '(rust . t) load-language-alist))

  ;; npm install -g @mermaid-js/mermaid-cli
  ;; (use-package ob-mermaid
  ;;   :init (cl-pushnew '(mermaid . t) load-language-alist))
  (org-babel-do-load-languages 'org-babel-load-languages load-language-alist)

  (use-package org-rich-yank
    :bind (:map org-mode-map
            ("C-M-y" . org-rich-yank)))

  (use-package org-preview-html
    :diminish
    :bind (:map org-mode-map
            ("C-c C-h" . org-preview-html-mode))
    :init (when (featurep 'xwidget-internal)
            (setq org-preview-html-viewer 'xwidget)))

  (use-package org-roam
    :diminish
    :custom
    (org-roam-directory (file-truename org-directory))
    (org-roam-capture-templates
      '(
         ("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
         ("b" "book notes" plain
           "\n* Source \n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
           :if-new (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))
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

    ;; If using org-roam-protocol
    (require 'org-roam-protocol)
    (require 'org-roam-export))

  (use-package org-roam-ui
    :after org-roam
    :custom
    (org-roam-ui-open-on-startup nil)
    (org-roam-ui-sync-theme t)
    (org-roam-ui-follow t)
    (org-roam-ui-update-on-save t)
    (org-roam-ui-open-on-start nil)
    (org-roam-ui-browser-function #'xwidget-webkit-browse-url))

  (use-package consult-org-roam :after (consult org-roam)))

(use-package org-contrib)

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package websocket :after (org-roam))

(use-package deft
  :config
  (setq deft-directory (file-truename org-directory)
    deft-recursive t
    deft-strip-summary-regexp  ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
    deft-use-filename-as-title t
    deft-auto-save-interval 0)
  :bind
  ("C-c n d" . deft))

  (use-package svg-tag-mode
    :commands svg-tag-mode
    :hook (org-mode . svg-tag-mode)
    :config
    (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
    (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
    (defconst day-re "[A-Za-z]\\{3\\}")
    (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

    (defun svg-progress-percent (value)
      (svg-image (svg-lib-concat
                   (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                     nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                   (svg-lib-tag (concat value "%")
                     nil :stroke 0 :margin 0)) :ascent 'center))

    (defun svg-progress-count (value)
      (let* ((seq (mapcar #'string-to-number (split-string value "/")))
              (count (float (car seq)))
              (total (float (cadr seq))))
        (svg-image (svg-lib-concat
                     (svg-lib-progress-bar (/ count total) nil
                       :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                     (svg-lib-tag value nil
                       :stroke 0 :margin 0)) :ascent 'center)))

    (setq svg-tag-tags
      `(
         ;; Org tags
         (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
         (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

         ;; Task priority
         ("\\[#[A-Z]\\]" . ( (lambda (tag)
                               (svg-tag-make tag :face 'org-priority
                                 :beg 2 :end -1 :margin 0))))

         ;; Progress
         ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                             (svg-progress-percent (substring tag 1 -2)))))
         ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                           (svg-progress-count (substring tag 1 -1)))))

         ;; TODO / DONE
         ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
         ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


         ;; Citation of the form [cite:@Knuth:1984]
         ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                           (svg-tag-make tag
                                             :inverse t
                                             :beg 7 :end -1
                                             :crop-right t))))
         ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                    (svg-tag-make tag
                                                      :end -1
                                                      :crop-left t))))


         ;; Active date (with or without day name, with or without time)
         (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
         (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
         (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

         ;; Inactive date  (with or without day name, with or without time)
         (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
         (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
         (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date)))))))

  ;; Auto toggle LaTeX rendering
  (use-package org-fragtog
    :hook (org-mode . org-fragtog-mode))

  (provide 'init-org)
;;; init-org.el ends here
