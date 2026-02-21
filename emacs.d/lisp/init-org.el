;; -*- lexical-binding: t; -*-

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 150)
  (visual-fill-column-center-text t)
  (visual-fill-column-enable-sensible-window-split nil)
  (visual-fill-column-fringes-outside-margins t)
  :config (global-visual-fill-column-mode))

(use-package org
  :defer
  :straight (org
             :fork (:host nil
                          :repo "https://git.tecosaur.net/tec/org-mode.git"
                          :branch "dev"
                          :remote "tecosaur")
             :files (:defaults "etc")
             :build t
             :pre-build
             (with-temp-file "org-version.el"
               (require 'lisp-mnt)
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents "lisp/org.el")
                        (lm-header "version")))
                     (git-version
                      (string-trim
                       (with-temp-buffer
                         (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                         (buffer-string)))))
                (insert
                 (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                 (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                 "(provide 'org-version)\n")))
              :pin nil)
  :bind ((:map org-mode-map
               ("M-g i" . consult-org-heading)))
  :init (setq org-directory (file-truename "~/Dropbox/notes"))
  :custom-face
  (org-list-dt ((t (:inherit default))))
  (org-level-1 ((t (:inherit outline-1 :height 1.4))))
  (org-level-2 ((t (:inherit outline-2 :height 1.25))))
  (org-level-3 ((t (:inherit outline-3 :height 1.15))))
  (org-level-4 ((t (:inherit outline-4 :height 1.05))))
  (org-document-title ((t (:height 1.6 :weight bold))))
  :hook
  ((org-babel-after-execute . org-redisplay-inline-images)
   (org-mode . (lambda ()
                 (mixed-pitch-mode)
                 (setq visual-fill-column-center-text nil)
                 (visual-fill-column-mode)))
   (org-mode . turn-on-org-cdlatex)
   (org-mode . word-wrap-whitespace-mode)
   (org-mode . org-latex-preview-mode))
  :config
  (setq org-startup-with-latex-preview t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(n)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c@/!)")))
  (setq org-tags-alist '(("inbox" . ?i)))
  ;; (org-agenda-start-with-log-mode t)
  ;; (org-log-into-drawer t)
  (setq org-log-done 'time) ; Record the task completion date.
  (setq org-pretty-entities t)
  (setq org-use-sub-superscripts '{})  ; only _{sub} triggers subscript, not every _
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation nil)        ; this to be nil to work well with org-modern-indent
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'split-window-below)
  (setq org-fontify-quote-and-verse-blocks t) ; highlight quote and verse blocks
  (setq org-fontify-whole-heading-line t)     ; highlight the whole line for headings
  (setq org-hide-emphasis-markers t)          ; org styling, hide markup etc
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
  (setq org-agenda-tags-column 0)

  ;; Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)
  (setq org-latex-preview-live-debounce 0.25) ;; More immediate live-previews
  (plist-put org-format-latex-options :scale 4.0)


  (add-hook 'org-mode-hook (lambda () (ws-butler-mode -1)))

  ;; Better org LaTeX preview
  ;; (setq org-startup-with-latex-preview t
  ;;       org-highlight-latex-and-related '(native latex))


  ;; (when (executable-find "dvisvgm")
  ;;   ;; Use dvisvgm for SVG LaTeX previews in Org-mode
  ;;   (setq org-latex-create-formula-image-program 'dvisvgm)
  ;;   (setq org-preview-latex-default-process 'dvisvgm))


  ;; (org-priority-faces
  ;;   '((?A . error)
  ;;      (?B . warning)
  ;;      (?C . success)))
  (setq org-link-elisp-confirm-function nil)
  (setq org-startup-with-inline-images t) ; always display images
  (setq org-confirm-babel-evaluate nil)   ; just evaluate

  (define-key global-map (kbd "C-c a") 'org-agenda)

  (setq org-agenda-files
        (directory-files-recursively (expand-file-name "agenda" org-directory) "\\.org$"))
  (setq org-refile-targets
        (quote ((nil :maxlevel . 9)
                (org-agenda-files :maxlevel . 9))))
  ;; (setq org-refile-targets `((,(expand-file-name "Dropbox/notes/todo.org" (getenv "HOME")) :maxlevel . 1))) ; Allow moving task from anywhere into todo
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/Dropbox/notes/inbox.org")
           "* TODO %?\n/Entered on/ %U\n" :clock-in t :clock-resume t)
          ("m" "Meeting" entry (file "~/Dropbox/notes/meetings.org")
           ,(concat "* %? :meeting:\n"
                    "<%<%Y-%m-%d %a %H:00>>"))
          ("j" "Journal" entry (file+olp+datetree "~/Dropbox/notes/journal.org")
           "* %?\n")))
  (setq org-agenda-window-setup 'current-window) ; Open agenda in current window
  (setq org-agenda-restore-windows-after-quit t) ; Restore window configuration after quitting agenda
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                   (todo   . " %i %-12:c")
                                   (tags   . " %i %-12:c")
                                   (search . " %i %-12:c")))

  (add-to-list 'org-src-lang-modes '("mermaid" . mermaid-ts))
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


  (defun org-archive-done-tasks ()
    "Archive all tasks marked DONE in the file. With auto save disabled."
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

  (require 'org-indent)



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

  (use-package ob-emacs-lisp :straight nil)

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
      (latex . t)
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
    :straight (ob-racket :type git :host github :repo "hasu/emacs-ob-racket" :files ("*.el" "*.rkt"))
    :after org
    :init (cl-pushnew '(racket . t) load-language-alist)
    :config
    (add-hook 'ob-racket-pre-runtime-library-load-hook
              #'ob-racket-raco-make-runtime-library))

  ;; npm install -g @mermaid-js/mermaid-cli
  (use-package ob-mermaid
    :if (executable-find "mmdc")
    :straight (ob-mermaid :type git :host github :repo "arnm/ob-mermaid")
    :init (cl-pushnew '(mermaid . t) load-language-alist)
    :config (setq ob-mermaid-cli-path (executable-find "mmdc")))

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
    :config (when (featurep 'xwidget-internal)
              (setq org-preview-html-viewer 'xwidget))
    :bind (:map org-mode-map
                ("C-c C-h" . org-preview-html-mode)))

  (use-package org-roam
    :straight (org-roam :type git :host github :repo "org-roam/org-roam"
                        :files (:defaults "extensions/*"))
    :init
    (setq org-roam-directory (file-truename org-directory))
    (setq org-roam-dailies-directory "daily/")
    (org-roam-db-autosync-mode)
    :custom
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
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:*}" 'face 'org-tag))))

  (use-package org-roam-protocol :straight nil :after org-roam)

  (use-package org-roam-export :straight nil :after org-roam)

  (use-package org-roam-ui
    :disabled t
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
  :after (evil)
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

(use-package org-appear
  :straight (org-appear :type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t)
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t)
  (setq org-appear-inside-latex t)
  (setq org-appear-trigger 'always))

(use-package org-journal
  :bind ("C-c n J" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: "))

(use-package org-download
  :defer t
  :after org
  :bind
  (:map org-mode-map
   (("s-Y" . org-download-screenshot)
    ("s-y" . org-download-yank))))

(use-package org-modern
  :custom
  (org-modern-hide-stars nil)
  (org-modern-table nil)
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(use-package org-modern-indent
  :straight (:type git :host github :repo "jdtsmith/org-modern-indent")
  :config (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-download)

(use-package consult-org-roam
  :diminish
  :after org-roam
  :init
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  ;; (consult-org-roam-buffer-after-buffers t)
  :config

  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-.")

  :bind
  ("C-c n e" . consult-org-roam-find-file)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search))

(use-package consult-notes
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :after org-roam
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  (consult-customize
   consult-notes
   consult-notes-search-in-all-notes
   consult-notes-org-roam-find-node
   consult-notes-org-roam-find-node-relation
   :preview-key "M-.")

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

(use-package denote
  :custom
  (denote-directory (file-truename "~/Dropbox/notes/denote")))

(use-package ox
  :straight (:type built-in)
  :config
  (setq org-export-with-smart-quotes t
        org-html-validation-link nil
        org-latex-prefer-user-labels t
        org-export-with-latex t))

(use-package ox-beamer
  :straight nil
  :after org)

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

(use-package orgit)

(use-package orgit-forge)

(use-package orgit-file
  :straight (:type git :host github :repo "gggion/orgit-file")
  :after (orgit))

(use-package org-visual-indent
    :after org
    :straight (:type git :host github :repo "legalnonsense/org-visual-outline" :files ("*.el"))
    ;; :hook (org-mode . org-visual-indent-mode)
    :config
    ;; Function to update indent colors for org-visual-indent
    (defun +org-visual-outline-indent-color-update (&rest _)
      "Update colors for org-visual-indent based on current theme."
      (let (bufs)
        ;; Collect buffers with org-visual-indent-mode enabled
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when org-visual-indent-mode
              (push buf bufs)
              (org-visual-indent-mode -1))))
        ;; Calculate color values for indent levels based on org-level faces
        (setq org-visual-indent-color-indent
              (cl-loop for x from 1 to 8
                       with color = nil
                       do (setq color (or (face-foreground (intern (concat "org-level-" (number-to-string x))) nil t)
                                          (face-foreground 'org-level-1)))
                       collect `(,x ,(list :background color :foreground color :height .1))))
        ;; Set face attributes for indent pipe faces
        (set-face-attribute 'org-visual-indent-pipe-face nil
                            :foreground (face-attribute 'default :foreground)
                            :background (face-attribute 'default :foreground))
        (set-face-attribute 'org-visual-indent-blank-pipe-face nil
                            :foreground (face-attribute 'default :background)
                            :background (face-attribute 'default :background))
        ;; Re-enable org-visual-indent-mode in collected buffers
        (dolist (buf bufs)
          (with-current-buffer buf
            (org-visual-indent-mode t)))))
    ;; Add the color update function to theme change hooks and call it immediately
    (add-hook 'enable-theme-functions #'+org-visual-outline-indent-color-update)
    ;; Call the function immediately to set initial colors
    (+org-visual-outline-indent-color-update))

(use-package org-expose-emphasis-markers
  :straight (:type git :host github :repo "lorniu/org-expose-emphasis-markers")
  :hook (org-mode . org-expose-emphasis-markers-mode))

(use-package org-remark
  :straight (org-remark :type git :host github :repo "nobiot/org-remark")
  :config
  (use-package org-remark-info :straight nil :after info :config (org-remark-info-mode +1))
  (use-package org-remark-nov :straight nil :after nov :config (org-remark-nov-mode +1))
  (require 'org-remark-global-tracking)
  (org-remark-global-tracking-mode t)
  ;; (use-package org-remark-eww :straight nil :after eww :config (org-remark-eww-mode +1))
  :bind (;; :bind keyword also implicitly defers org-remark itself.
         ;; Keybindings before :map is set for global-map. Adjust the keybinds
         ;; as you see fit.
         ("C-c l m" . org-remark-mark)
         ("C-c l l" . org-remark-mark-line)
         :map org-remark-mode-map
         ("C-c l o" . org-remark-open)
         ("C-c l ]" . org-remark-view-next)
         ("C-c l [" . org-remark-view-prev)
         ("C-c l r" . org-remark-remove)
         ("C-c l d" . org-remark-delete)))

(use-package org-drill
  :disabled t
  :straight (:type git :host gitlab :repo "phillord/org-drill")
  :after org)


;; (use-package org-fc
;;   :straight (:type git :repo "https://git.sr.ht/~l3kn/org-fc" :files (:defaults "awk" "demo.org"))
;;   :custom (org-fc-directories '("~/org/"))
;;   :config (require 'org-fc-hydra))

(use-package codetabs
  :disabled t
  :after org
  :straight (:type git :host github :repo "Clement-Jean/codetabs.el" :files ("codetabs.el" "codetabs.js")))

(use-package org-backlinks
  :straight (org-backlinks :host github :repo "bcardoso/org-backlinks"
                           :files ("org-backlinks.el"))
  :bind ("C-c z o" . org-backlinks))

(use-package consult-org-backlinks
  :straight (consult-org-backlinks :host github :repo "bcardoso/org-backlinks"
                                   :files ("consult-org-backlinks.el"))
  :bind ("C-c z c" . consult-org-backlinks))

(use-package org-transclusion
  :straight (:type git :host github :repo "nobiot/org-transclusion")
  :after org
  :custom
  (org-transclusion-add-all-on-activate t)
  :config
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  (require 'org-transclusion-indent-mode)
  (general-define-key
   :keymaps '(org-transclusion-map)
   :states 'normal
   "RET" #'org-transclusion-open-source
   "gr" #'org-transclusion-refresh)
  (general-define-key
   :keymaps '(org-mode-map)
   :states 'normal
   "C-c t a" #'org-transclusion-add
   "C-c t A" #'org-transclusion-add-all
   "C-c t t" #'org-transclusion-mode
   "C-c t r" #'org-transclusion-remove))


(use-package org-transclusion-orgit
  :straight (org-transclusion-orgit :type git :host github :repo "gggion/org-transclusion-orgit")
  :after (org-transclusion))

(use-package org-transclusion-blocks
  :straight (:type git :host github :repo "gggion/org-transclusion-blocks"))

(use-package verb :after org
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-v") verb-command-map)))

(provide 'init-org)
;;; init-org.el ends here
