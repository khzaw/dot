(use-package magit
  :bind ("C-x g" . magit-status)
  :init (setq magit-diff-refine-hunk t) ;; show granular diffs in selected hunk
  :custom
  (magit-auto-revert-mode t)
  :config

  ;; (customize-set-variable
  ;;   'display-buffer-alist
  ;;   '(("\\*magit: .*" display-buffer-same-window)))
  ;; Suppress the message
                                        ;
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  ;; (setq magit-bury-buffer-function #'quit-window) ;; let shackle handle this
  (setq magit-bury-buffer-function #'magit-mode-quit-window)

  (setq magit-refresh-status-buffer nil)
  ;; Don't display parent/related refs in commit buffers
  (setq magit-revision-insert-related-refs nil)

  (setq magit-save-repository-buffers nil ;; don't autosave repo buffers.
        display-line-numbers-type 'visual
        markdown-display-remote-images t
        magit-section-disable-line-numbers nil))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode)
  :disabled t
  :config
  (add-to-list 'magit-delta-delta-args "--light" "--no-gitconfig"))

(use-package magit-todos
  :after magit)

(setq auth-sources (list
                    (concat (getenv "XDG_CONFIG_HOME") "/authinfo.gpg")
                    "~/.authinfo"
                    "~/.authinfo.gpg"))

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
  (setq forge-topic-list-limit '(60 . 5)))

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

(use-package git-modes)

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
  (setq consult-gh-default-orgs-list '("khzaw" "projectrangoon" "algo-koans" "deliveryhero"))
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

(use-package ediff
  :straight (:type built-in)
  :hook
  ;; show org ediffs unfolded
  (edit-prepare-buffer . outline-show-all)
  ;; restore window layout when done
  (ediff-quit . winner-undo))

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

(use-package ov
  :straight (:type git :host github :repo "emacsorphanage/ov"))

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

(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))

(provide 'init-vcs)
;;; init-vcs.el ends here
