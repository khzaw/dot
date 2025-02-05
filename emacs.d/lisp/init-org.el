;; -*- lexical-binding: t; -*-

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t)
  (visual-fill-column-split-window-sensibly t))

(use-package org
  :bind (("C-c C-c" . org-edit-src-exit))
  :init
  (setq org-directory (file-truename "~/Dropbox/notes"))
  :hook
  ((org-babel-after-execute . org-redisplay-inline-images)
   (org-mode . (lambda ()
                 (variable-pitch-mode)
                 (setq visual-fill-column-center-text nil)
                 (visual-fill-column-mode))))
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(n)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c@/!)")))
  (setq org-tags-alist '(("inbox" . ?i)))
  ;; (org-agenda-start-with-log-mode t)
  ;; (org-log-into-drawer t)
  (setq org-log-done 'time) ; Record the task completion date.
  (setq org-pretty-entities t)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)        ; use native major-mode indentation
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'other-window)
  (setq org-hide-emphasis-markers t)
  (setq org-startup-truncated nil)
  (setq org-imenu-depth 6)
  (setq org-startup-indented t)
  (setq org-startup-folded t)
  (setq org-special-ctrl-a/e t)
  (setq org-link-search-must-match-exact-headline nil)
  (setq org-M-RET-may-split-line nil)
  (setq org-auto-align-tags t)
  (setq org-tags-column 0)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-insert-heading-respect-content t) ; insert new headings after current subtree rather than inside it
  ;; (org-priority-faces
  ;;   '((?A . error)
  ;;      (?B . warning)
  ;;      (?C . success)))
  (setq org-link-elisp-confirm-function nil)
  (setq org-startup-with-inline-images t) ; always display images
  (setq org-confirm-babel-evaluate nil) ; just evaluate
  (setq org-refile-targets `((,(expand-file-name "Dropbox/notes/todo.org" (getenv "HOME")) :maxlevel . 1))) ; Allow moving task from anywhere into todo
  (setq org-capture-templates
        '(("t" "todo" entry (file "~/Dropbox/notes/inbox.org")
           "* TODO %?\n/Entered on/ %U\n")
          ("j" "Journal" entry (file+olp+datetree "~/Dropbox/notes/journal.org")
           "* %?\n")))
  (setq org-agenda-window-setup 'current-window) ; Open agenda in current window
  (setq org-agenda-restore-windows-after-quit t) ; Restore window configuration after quitting agenda
  (setq org-agenda-files
        (directory-files-recursively (expand-file-name "agenda" org-directory) "\\.org$"))

  (add-to-list 'org-src-lang-modes '("mermaid" . mermaid-ts))
  (global-set-key (kbd "<f6>") 'org-capture)
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  ;; The GTD view
  (setq-default org-agenda-custom-commands
                '(("g" "Get Things Done (GTD)"
                   ((agenda ""
                            ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                             (org-deadline-warning-days 0)))
                    (todo "DOING"  ; Changed from NEXT to DOING to match your keywords
                          ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                           (org-agenda-prefix-format "  %i %-12:c [%e] ")
                           (org-agenda-overriding-header "\nOngoing Tasks\n")))
                    (todo "TODO"
                          ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                           (org-agenda-prefix-format "  %i %-12:c [%e] ")
                           (org-agenda-overriding-header "\nTasks\n")))
                    (tags-todo "inbox"
                               ((org-agenda-prefix-format "  %?-12t% s")
                                (org-agenda-overriding-header "\nInbox\n")))
                    (tags "CLOSED>=\"<today>\""
                          ((org-agenda-overriding-header "\nCompleted today\n")))))))

  ;; Press F4 to get things done!
  (global-set-key (kbd "<f4>") (lambda () (interactive) (org-agenda nil "g")))


  ;; With auto save disabled
  (defun org-archive-done-tasks ()
    "Archive all tasks marked DONE in the file."
    (interactive)
    ;; Disable auto save to avoid repeated file write.
    (setq org-archive-subtree-save-file-p nil)
    ;; unwind-protect is like try/finally
    (unwind-protect
        ;; process the entry in reverse to avoid changes in positioning
        (mapc (lambda(entry)
                (goto-char entry)
                (org-archive-subtree))
              (reverse (org-map-entries (lambda () (point)) "TODO=\"DONE\"" 'file)))
      ;; Enable auto save, even if an exception is raised.
      (setq org-archive-subtree-save-file-p t))
    (org-save-all-org-buffers))


  ;; (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (require 'org-indent)

  (setq sentence-end-double-space nil)


  (use-package org-contrib)

  (defun khz/org-link-copy (&optional arg)
    "Extract URL from org-mode link and add it to kill ring."
    (interactive "P")
    (let* ((link (org-element-lineage (org-element-context) '(link) t))
           (type (org-element-property :type link))
           (url (org-element-property :path link))
           (url (concat type ":" url)))
      (kill-new url)
      (message (concat "Copied URL: " url))))

  (define-key org-mode-map (kbd "C-x C-l") 'khz/org-link-copy)

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist
               '("m" . "src mermaid :file %^{filename}.png :exports results"))


  (use-package ox-gfm :after org)
  (add-to-list 'org-export-backends 'md)

  (setq org-babel-shell-names '("sh" "bash" "zsh")
        org-babel-default-header-args:shell
        `((:shebang . ,(format "#!/usr/bin/env %s"
                               (or (executable-find "zsh") "bash")))))
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
  (use-package ob-shell
    :straight nil
    :after org
    :init (cl-pushnew '(shell . t) load-language-alist))

  (use-package ob-go
    :after org
    :if (executable-find "go")
    :init (cl-pushnew '(go . t) load-language-alist))

  (use-package ob-rust
    :after org
    :if (executable-find "rustc")
    :init (cl-pushnew '(rust . t) load-language-alist))

  (use-package ob-racket
    :after org
    :config
    (add-hook 'ob-racket-pre-runtime-library-load-hook
              #'ob-racket-raco-make-runtime-library)
    :straight (ob-racket :type git :host github :repo "hasu/emacs-ob-racket" :files ("*.el" "*.rkt"))
    :init (cl-pushnew '(racket . t) load-language-alist))

  ;; npm install -g @mermaid-js/mermaid-cli
  (use-package ob-mermaid
    :if (executable-find "mmdc")
    :straight (ob-mermaid :type git :host github :repo "arnm/ob-mermaid")
    :init (cl-pushnew '(mermaid . t) load-language-alist))

  (use-package ob-dot
    :if (executable-find "dot")
    :straight nil
    :commands (org-babel-execute:go:dot org-babel-expand-body:dot)
    :init (cl-pushnew '(dot . t) load-language-alist))

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
    :straight (org-roam :type git :host github :repo "org-roam/org-roam"
                        :files (:defaults "extensions/*"))
    :custom
    (org-roam-directory (file-truename org-directory))
    (org-roam-completion-everywhere t)
    (org-roam-capture-templates
     '(("d" "default" plain
        "%?"
        :if-new (file+head "personal/${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+startup: showall\n")
        :immediate-finish t
        :empty-lines 1
        :unnarrowed t)

       ("t" "tech" plain
        "%?"
        :if-new (file+head "tech/${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+startup: showall\n")
        :immediate-finish t
        :empty-lines 1
        :unnarrowed t)

       ("b" "books" plain
        "\n* Source \n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
        :if-new (file+head "books/${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+filetags: books\n#+startup: showall\n")
        :immediate-finish t
        :empty-lines 1
        :unnarrowed t)

       ("w" "work" plain
        "%?"
        :if-new (file+head "fp/${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+updated: \n\n")
        :immediate-finish t
        :empty-lines 1
        :unnarrowed t)))

    :bind (("C-c n g" . org-roam-graph)
           ("C-c n c" . org-roam-capture)
           ;; Dailies
           ("C-c n j" . org-roam-dailies-capture-today)
           (:map org-mode-map
            (("C-c n i" . org-roam-node-insert)
             ("C-c n o" . org-id-get-create)
             ("C-c n t" . org-roam-tag-add)
             ("C-c n a" . org-roam-alias-add)
             ("C-c n l" . org-roam-buffer-toggle))))
    :config
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:*}" 'face 'org-tag)))
    (setq org-roam-dailies-directory "daily/")
    (org-roam-db-autosync-mode)
    (toggle-word-wrap))

  (use-package org-roam-protocol
    :straight nil
    :after org-roam)

  (use-package org-roam-export :straight nil :after org-roam)

  (use-package org-roam-ui
    :after org-roam
    :custom
    (org-roam-ui-sync-theme t)
    (org-roam-ui-follow t)
    (org-roam-ui-update-on-save t)
    (org-roam-ui-open-on-start nil)
    (org-roam-ui-browser-function #'xwidget-webkit-browse-url)))

(use-package quickroam
  :straight (quickroam :type git :host github :repo "meedstrom/quickroam")
  :defer t
  :hook (org-mode . quickroam-enable-cache))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package websocket :after (org-roam))

(use-package deft
  :after org
  :config
  (setq deft-directory (file-truename org-directory)
        deft-recursive t
        deft-strip-summary-regexp  ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title t
        deft-default-extension "org"
        deft-auto-save-interval 0)
  :bind
  ("C-c n d" . deft))

(use-package org-journal
  :bind ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: "))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
   (("s-Y" . org-download-screenshot)
    ("s-y" . org-download-yank))))

;; Auto toggle LaTeX rendering
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-modern
  :config
  (with-eval-after-load 'org (global-org-modern-mode)))

(use-package org-download)

(use-package consult-org-roam
  :init
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  ;; (consult-org-roam-buffer-after-buffers t)
  :config
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  :bind
  ("C-c n e" . consult-org-roam-find-file)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search))

(use-package consult-notes
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  (setq consult-notes-file-dir-sources
        '(("fp"  ?w  "~/Dropbox/notes/fp")
          ("tech" ?t "~/Dropbox/notes/tech")
          ("books" ?b "~/Dropbox/notes/books"))) ;; Set notes dir(s), see below
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  ;; (setq consult-notes-org-headings-files '("~/path/to/file1.org"
  ;;                                           "~/path/to/file2.org"))
  (consult-notes-org-headings-mode)
  (consult-notes-org-roam-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  :bind
  ("C-c n f" . consult-notes-org-roam-find-node))

(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

(use-package denote)

(use-package org-ref
  :config
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))

(use-package org-appear
  :straight (org-appear :type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))

(use-package orgit)

(use-package orgit-forge)

(provide 'init-org)
;;; init-org.el ends here
