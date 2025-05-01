;; -*- lexical-binding: t; -*-

(use-package magit
  :init
  (setq-default magit-git-executable (executable-find "git"))
  :defer t
  :commands (magit-status magit-blame magit-get-current-branch)
  :bind ("C-x g" . magit-status)
  :config

  ;; (customize-set-variable
  ;;   'display-buffer-alist
  ;;   '(("\\*magit: .*" display-buffer-same-window)))
  ;; Suppress the message
  (setq magit-auto-revert-mode t)

  (setq magit-diff-refine-hunk t) ;; show granular diffs in selected hunk
                                        ;
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  ;; (setq magit-bury-buffer-function #'quit-window) ;; let shackle handle this
  (setq magit-bury-buffer-function #'magit-mode-quit-window)

  ;; don't refresh status buffer automatically
  (setq magit-refresh-status-buffer nil)
  ;; Don't display parent/related refs in commit buffers
  (setq magit-revision-insert-related-refs nil)

  (setq magit-save-repository-buffers 'dontask
        display-line-numbers-type 'visual
        markdown-display-remote-images t
        magit-section-disable-line-numbers nil)

  (setq magit-section-initial-visibility-alist
        '((untracked . show)
          (unstaged . show)
          (staged . show)
          (stashes . hide)))

  ;; Just type C-c C-d to show the diff at committing
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode)
  :disabled t
  :config
  (add-to-list 'magit-delta-delta-args "--light" "--no-gitconfig"))

(use-package magit-todos
  :disabled t  ;; super slow in big repos
  :after magit
  :config (magit-todos-mode 1))

(setq auth-sources (list
                    (concat (getenv "XDG_CONFIG_HOME") "/authinfo.gpg")
                    "~/.authinfo"
                    "~/.authinfo.gpg"))

(use-package magit-blame-color-by-age
  :straight (:repo "jdtsmith/magit-blame-color-by-age" :host github))

(use-package git-commit
  :straight nil
  :config
  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line)
  (global-git-commit-mode))

(use-package diff-hl
  :custom (diff-refine 'navigation)
  :after evil-leader
  :config
  ;; (diff-hl-flydiff-mode)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (setq vc-git-diff-switches '("--histogram"))
  (setq diff-hl-disable-on-remote t)
  (global-diff-hl-mode)
  (evil-set-command-property 'diff-hl-revert-hunk :jump t)
  (evil-set-command-property 'diff-hl-next-hunk :jump t)
  (evil-set-command-property 'diff-hl-previous-hunk :jump t)
  (evil-leader/set-key "gs" 'diff-hl-show-hunk)
  (evil-leader/set-key "gr" 'diff-hl-revert-hunk)
  (evil-leader/set-key "gj" 'diff-hl-next-hunk)
  (evil-leader/set-key "gk" 'diff-hl-previous-hunk)
  (evil-leader/set-key "gn" 'diff-hl-next-hunk)
  (evil-leader/set-key "gp" 'diff-hl-previous-hunk))

(use-package difftastic
  :if (executable-find "difft")
  :straight (:type git :host github :repo "pkryger/difftastic.el")
  :init
  (use-package transient
    :autoload (transient-get-suffix
               transient-parse-suffix))
  (let ((suffix [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
                 ("S" "Difftastic show" difftastic-magit-show)]))
    (use-package magit-blame
      :straight nil
      :bind
      (:map magit-blame-read-only-mode-map
            ("D" . #'difftastic-magit-diff)
            ("S" . #'difftastic-magit-show))
      :config
      (unless (equal (transient-parse-suffix 'magit-blame suffix)
                     (transient-get-suffix 'magit-blame '(-1)))
        (transient-append-suffix 'magit-blame '(-1) suffix)))
    (use-package magit-diff
      :straight nil
      :config
      (unless (equal (transient-parse-suffix 'magit-diff suffix)
                     (transient-get-suffix 'magit-diff '(-1 -1)))
        (transient-append-suffix 'magit-diff '(-1 -1) suffix)))))

(use-package difftastic-bindings
  :after difftastic
  :straight nil
  :config (difftastic-bindings-mode))

(use-package forge
  :after magit
  :init
  (setq forge-get-repository-verbose t)
  (setq forge-add-default-bindings nil) ;; will be take care of by evil-collection -> forge
  :config
  ;; A topic is an issue or PR and the list of each can be configured
  ;; to display a number of open and closed items.
  ;; Show 100 open topics and never show any closed topics, for both
  ;; issues and PRs.
  (setq forge-topic-list-limit '(60 . 5))

  (defun khz/forge-browse-buffer-file ()
    (interactive)
    (browse-url
     (let
         ((rev (magit-rev-abbrev "HEAD"))
          (repo (forge-get-repository 'stub))
          (file (magit-file-relative-name buffer-file-name))
          (highlight
           (if
               (use-region-p)
               (let ((l1 (line-number-at-pos (region-beginning)))
                     (l2 (line-number-at-pos (- (region-end) 1))))
                 (format "#L%d-L%d" l1 l2))
             ""
             )))
       (forge--format repo "https://%h/%o/%n/blob/%r/%f%L"
                      `((?r . ,rev) (?f . ,file) (?L . ,highlight))))))

  (global-set-key (kbd "C-c g L") 'khz/forge-browse-buffer-file))

(use-package code-review
  :straight (:type git :host github :repo "phelrine/code-review" :branch "fix/closql-update")
  :after (forge emojify)
  :hook (code-review-mode . emojify-mode)
  :config
  (setq code-review-fill-column 120)
  (setq code-review-auth-login-marker 'forge)
  (transient-append-suffix 'magit-merge "i" '("y" "Review pull request" code-review-forge-pr-at-point))
  (transient-append-suffix 'forge-dispatch "c u" '("c r" "Review pull-request" code-review-forge-pr-at-point))
  :bind ((:map magit-status-mode-map
          ("C-c r" . code-review-forge-pr-at-point))
         (:map code-review-mode-map
          ("r" . code-review-transient-api)
          ("gr" . code-review-reload)
          ("RET" . code-review-comment-add-or-edit)
          ("d" . code-review-submit-single-diff-comment-at-point))))

(use-package github-review
  :after forge
  :commands (github-review-start github-review-forge-pr-at-point))

;; (use-package pr-review
;;   :straight (:type git :host github :repo "blahgeek/emacs-pr-review")
;;   :config
;;   (evil-ex-define-cmd "prr" #'pr-review)
;;   (evil-ex-define-cmd "prs" #'pr-review-search)
;;   (evil-ex-define-cmd "prn" #'pr-review-notification)
;;   (add-to-list 'browse-url-default-handlers
;;                '(pr-review-url-parse . pr-review-open-url)))

(use-package git-timemachine
  :bind (:map vc-prefix-map
         ("t" . git-timemachine-toggle)))

(use-package git-messenger
  :bind ("C-c g m" . git-messenger:popup-message)
  :init (setq git-messenger:show-detail t
              git-messenger:use-magit-popup t))
;; :config
;; (progn
;;   (define-key git-messenger-map (kbd "RET") 'git-messenger:popup-close)))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :bind (:map vc-prefix-map
          ("B" . browse-at-remote)))

(use-package git-modes
  :init
  (add-to-list 'auto-mode-alist '("gitignore_global\\'" . gitignore-mode)))

(use-package gitignore-templates)

(use-package git-gutter
  :bind (:map vc-prefix-map
         ("C-g" . git-gutter-mode)
         ("p" . git-gutter:previous-hunk)
         ("n" . git-gutter:next-hunk)))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package blamer
  :straight (:type git :host github :repo "artawower/blamer.el")
  :bind (("C-c g i" . blamer-show-commit-info)
         ("C-c g p" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-view 'overlay-right)
  (blamer-idle-time 0.3)
  (blamer-min-offset 70))

(use-package consult-gh
  :straight (consult-gh :type git :host github :repo "armindarvish/consult-gh" :files (:defaults "*.el"))
  :after consult
  :config
  (require 'consult-gh-transient)
  (require 'consult-gh-embark)
  (require 'consult-gh-forge)
  (consult-gh-embark-mode +1)
  (consult-gh-forge-mode +1)
  (setq consult-gh-default-clone-directory "~/Code")
  (setq consult-gh-show-preview t
        consult-gh-preview-key "M-.")
  (setq consult-gh-issue-action #'consult-gh--issue-view-action ;; view issues inside emacs
        consult-gh-repo-action #'consult-gh--repo-browse-files-action ;; browse files inside emacs
        consult-gh-file-action #'consult-gh--files-view-action) ;; open files in an emacs buffer
  (add-to-history 'savehist-additional-variables 'consult-gh--known-orgs-list) ;; keep record of searched orgs
  (add-to-history 'savehist-additional-variables 'consult-gh--known-repos-list)) ;; keep record of searched repos

(use-package consult-git-log-grep
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

(use-package conventional-commit
  :straight (:host github :repo "akirak/conventional-commit.el")
  :hook
  (git-commit-mode . conventional-commit-setup))

(use-package git-link
  :straight (:type git :host github :repo "sshaw/git-link")
  :config
  (defun git-link-diffrent-branch (branch)
    "Invoke `git-link', but with the `branch' name set to a different
branch than the one you're currently working on."
    (interactive "P")
    (let* ((default-remote-branch-name "master")
           (git-link-current-branch-setting git-link-default-branch)
           (git-link-default-branch (if branch
                                        (completing-read
                                         (format "Instead of '%s' branch replace with branch: " (git-link--branch))
                                         (magit-list-branch-names))
                                      default-remote-branch-name)))
      (setq current-prefix-arg nil)
      (call-interactively 'git-link)
      (setq git-link-default-branch git-link-current-branch-setting)))
  (global-set-key (kbd "C-c g l") 'git-link))

(use-package magit-town
  :straight (:type git :host github :repo "khzaw/magit-town"))

(use-package magit-pretty-graph
  :straight (:type git :host github :repo "georgek/magit-pretty-graph"))

(defun unpackaged/magit-log--add-date-headers (&rest _ignore)
  "Add date headers to Magit log buffers."
  (when (derived-mode-p 'magit-log-mode)
    (save-excursion
      (ov-clear 'date-header t)
      (goto-char (point-min))
      (cl-loop with last-age
               for this-age = (-some--> (ov-in 'before-string 'any (line-beginning-position) (line-end-position))
                                car
                                (overlay-get it 'before-string)
                                (get-text-property 0 'display it)
                                cadr
                                (s-match (rx (group (1+ digit) ; number
                                                    " "
                                                    (1+ (not blank))) ; unit
                                             (1+ blank) eos)
                                         it)
                                cadr)
               do (when (and this-age
                             (not (equal this-age last-age)))
                    (ov (line-beginning-position) (line-beginning-position)
                        'after-string (propertize (concat " " this-age "\n")
                                                  'face 'magit-section-heading)
                        'date-header t)
                    (setq last-age this-age))
               do (forward-line 1)
               until (eobp)))))

(defun unpackaged/magit-log--add-date-headers (&rest _ignore)
  ;; https://github.com/alphapapa/unpackaged.el?tab=readme-ov-file#magit-log-date-headers
  "Add date headers to Magit log buffers."
  (when (derived-mode-p 'magit-log-mode)
    (save-excursion
      (ov-clear 'date-header t)
      (goto-char (point-min))
      (cl-loop with last-age
               for this-age = (-some--> (ov-in 'before-string 'any (line-beginning-position) (line-end-position))
                                car
                                (overlay-get it 'before-string)
                                (get-text-property 0 'display it)
                                cadr
                                (s-match (rx (group (1+ digit) ; number
                                                    " "
                                                    (1+ (not blank))) ; unit
                                             (1+ blank) eos)
                                         it)
                                cadr)
               do (when (and this-age
                             (not (equal this-age last-age)))
                    (ov (line-beginning-position) (line-beginning-position)
                        'after-string (propertize (concat " " this-age "\n")
                                                  'face 'magit-section-heading)
                        'date-header t)
                    (setq last-age this-age))
               do (forward-line 1)
               until (eobp)))))

(define-minor-mode unpackaged/magit-log-date-headers-mode
  "Display date/time headers in `magit-log' buffers."
  :global t
  (if unpackaged/magit-log-date-headers-mode
      (progn
        ;; Enable mode
        (add-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
        (advice-add #'magit-setup-buffer-internal :after #'unpackaged/magit-log--add-date-headers))
    ;; Disable mode
    (remove-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
    (advice-remove #'magit-setup-buffer-internal #'unpackaged/magit-log--add-date-headers)))

(require 'hydra)
;; Resolve diff3 conflicts
(use-package smerge-mode
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

(use-package ediff
  :straight (:type built-in)
  :custom
  (ediff-diff-options "-w")
  :config
  (add-hook 'ediff-quit-hook 'winner-undo)

  ;; Custom function to kill the ediff buffers on quit
  (defun khz/ediff-cleanup-hook ()
    (when (and ediff-buffer-A (buffer-live-p ediff-buffer-A))
      (kill-buffer ediff-buffer-A))
    (when (and ediff-buffer-B (buffer-live-p ediff-buffer-B))
      (kill-buffer ediff-buffer-B))
    (when (and ediff-buffer-C (buffer-live-p ediff-buffer-C))
      (kill-buffer ediff-buffer-C)))

  (add-hook 'ediff-quit-hook 'khz/ediff-cleanup-hook))

(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package magit-tbdiff
  :after magit)

(use-package consult-vc-modified-files
  :straight (:type git :host github :repo "chmouel/consult-vc-modified-files")
  :bind
  ("C-x v /" . consult-vc-modified-files))

(use-package eldoc-diffstat
  :straight (:type git :host github :repo "kljohann/eldoc-diffstat")
  :config (global-eldoc-diffstat-mode))

(use-package igist
  :straight (igist
             :repo "KarimAziev/igist"
             :type git
             :host github))

(provide 'init-vcs)
;;; init-vcs.el ends here
