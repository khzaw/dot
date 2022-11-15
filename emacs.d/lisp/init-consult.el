(use-package consult
  :bind (
          ("C-c h" . consult-history)
          ("C-c m" . consult-mode-command)
          ("C-c k" . consult-kmacro)
          ("C-x M-:" . consult-complex-command)
          ("C-x b" . consult-buffer)
          ("C-x 4 b" . consult-buffer-other-window)
          ("C-x 5 b" . consult-buffer-other-frame)
          ("C-x r b" . consult-bookmark)
          ("C-x p b" . consult-project-buffer)
          ("M-#" . consult-register-load)
          ("M-'" . consult-register-store)
          ("C-M-#" . consult-register)
          ("M-y" . consult-yank-pop)
          ("<help> a" . consult-apropos)
          ("M-g e" . consult-compile-error)
          ("M-g f" . consult-flycheck)
          ("M-g g " . consult-goto-line)
          ("M-g M-g" . consult-goto-line)
          ("M-g o" . consult-outline)
          ("M-g m" . consult-mark)
          ("M-g k" . consult-global-mark)
          ("M-g i" . consult-imenu)
          ("M-g I" . consult-imenu-multi)
          ("M-s d" . consult-find)
          ("M-s D" . consult-locate)
          ("M-s g" . consult-grep)
          ("M-s G" . consult-git-grep)
          ("M-s r" . consult-ripgrep)
          ("M-s l" . consult-line)
          ("M-s L" . consult-line-multi)
          ("M-s m" . consult-multi-occur)
          ("M-s k" . consult-kep-lines)
          ("M-s u" . consult-focus-lines)
          ("M-s e" . consult-isearch-history)
          :map isearch-mode-map
          ("M-e" . consult-isearch-history)
          ("M-s e" . consult-isearch-history)
          ("M-s l" . consult-line)
          ("M-s L" . consult-line-multi)
          ;; Minibuffer history
          :map minibuffer-local-map
          ("M-s" . consult-history)
          ("M-r" . consult-history))
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
    register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :configu section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure the preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
    consult-theme :preview-key '(:debounce 0.2 any)
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-recent-file consult-xref
    consult--source-bookmark consult--source-file-register
    consult--source-recent-file consult--source-project-recent-file
    ;; :preview-key (kbd "M-.")
    :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(provide 'init-consult)
;;; init-consult.el ends here
