(use-package consult
  :config
  :bind (;; C-c bindings (mode-specific-map)
          ("C-c a" . consult-ag)
          ("C-c h" . consult-history)
          ("C-c i" . imenu)
          ("C-c m" . consult-mode-command)
          ("C-c k" . consult-kmacro)
          ("C-c l t" . consult-theme)
          ;; C-x bindings (ctl-x-map)
          ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
          ("C-x b" . consult-buffer) ;; orig . switch-to-buffer
          ("C-x 4 b" . consult-buffer-other-window)
          ("C-x 5 b" . consult-buffer-other-frame)
          ("C-x r b" . consult-bookmark)
          ("C-x p b" . consult-project-buffer)
          ;; Custom M-# bindings for fast register access
          ("M-#" . consult-register-load)
          ("M-'" . consult-register-store)
          ("C-M-#" . consult-register)
          ;; Other custom bindings
          ("M-y" . consult-yank-pop)
          ("<help> a" . consult-apropos)
          ;; M-g bindings (goto-map)
          ("M-g e" . consult-compile-error)
          ("M-g f" . consult-flycheck)
          ("M-g g" . consult-goto-line)
          ("M-g M-g" . consult-goto-line)
          ("M-g o" . consult-outline)
          ("M-g m" . consult-mark)
          ("M-g k" . consult-global-mark)
          ("M-g i" . consult-imenu)
          ("M-g I" . consult-imenu-multi)
          ;; M-s bindings (search-map)
          ("M-s d" . consult-find)
          ("C-c f" . consult-find)
          ("M-s D" . consult-locate)
          ("M-s g" . consult-grep)
          ("M-s G" . consult-git-grep)
          ("M-s r" . consult-ripgrep)
          ("C-s" . consult-line)
          ("M-s L" . consult-line-multi)
          ("M-s m" . consult-multi-occur)
          ("M-s k" . consult-keep-lines)
          ("M-s u" . consult-focus-lines)
          ;; Isearch integration
          ("M-s e" . consult-isearch-history)
          :map isearch-mode-map
          ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
          ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
          ("M-s l" . consult-line)       ;; needed by consult-line to detect isearch
          ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
          ;; Minibuffer history
          :map minibuffer-local-map
          ("M-s" . consult-history) ;; orig. next-matching-history-element
          ("M-r" . consult-history) ;; orig. previous-matching-history-element
          )

  ;; Enable automatic preview at point in the *Completion* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
    register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq
    xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (use-package consult-ag)

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
    consult-theme :preview-key '(:debounce 0.2 any)
    consult-ripgrep consult-git-grep consult-grep
    consult-ag consult-bookmark consult-recent-file consult-xref
    consult--source-bookmark consult--source-file-register
    consult--source-recent-file consult--source-project-recent-file
    ;; preview-key (kdb "M-.")
    :preview-key '(:debounce 0.4 any)
    )

  ;; Optionally configure the narrowing key.
  ;; Both < and C++ work resonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optinally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (autoload 'projectfile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package consult-spotify
  :after consult)

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq-default prescient-history-length 1000)
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
    history-length 1000
    savehist-additional-variables '(mark-ring
                                     global-mark-ring
                                     search-ring
                                     regexp-search-ring
                                     extended-command-history)
    savehist-autosave-interval 300))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-scroll-margin 0) ;; different scroll margin
  (vertico-resize t)
  (vertico-cycle t))

(use-package vertico-posframe
  :after (vertico posframe)
  :config (setq vertico-posframe-border-width 15)
  (vertico-posframe-mode))

;; Enable rich annotations in completion UI
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package embark
  :bind (("C-c C-." . embark-act)
          ("C-." . embark-act)
          ("C-;" . embark-dwim)
          ("C-c C-;" . embark-dwim)
          ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
    '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
       nil
       (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; manual preview for non-consult commands using embark
(define-key minibuffer-local-map (kbd "M-.") #'my-embark-preview)
(defun my-embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))


(use-package orderless
  :init
  ;; Configure a custom style dispatcher)
  (setq orderless-style-dispatchers '(+orderless-dispatch)
    orderless-component-separator #'orderless-escapable-split-on-space)

  (setq completion-styles '(basic substring partial-completion flex)
    completion-category-defaults nil
    completion-category-overrides '((file (styles partial-completion)))))

;; (use-package orderless
;;   :init (icomplete-mode)
;;   (setq orderless-style-dispatchers '(+orderless-dispatch))
;;   :custom (completion-styles '(basic substring partial-completion flex orderless)))

(provide 'init-consult)
;;; init-consult.el ends here
