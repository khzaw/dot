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

(use-package consult
  :after projectile
  :bind (("C-c a" . consult-ag)
          ("C-c h" . consult-history)
          ("C-c i" . consult-imenu)
          ("C-c r". consult-recent-file)
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
          ("M-g !" . consult-flymake)
          ("M-g f" . consult-flycheck)
          ("M-g M-g" . consult-goto-line)
          ("s-l" . consult-goto-line)
          ("M-g o" . consult-outline)
          ("M-g m" . consult-mark)
          ("M-g k" . consult-global-mark)
          ("M-g i" . consult-imenu)
          ("M-g I" . consult-imenu-multi)
          ;; M-s bindings (search-map)
          ("M-s f" . consult-find)
          ;; ("C-c f" . consult-find)
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

  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))

  (consult-customize consult-line :keymap my-consult-line-map)

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
    consult-theme :preview-key '(:debounce 0.5 any)
    consult-ripgrep consult-git-grep consult-grep
    consult-ag consult-bookmark consult-recent-file consult-xref
    consult--source-bookmark consult--source-file-register
    consult--source-recent-file consult--source-project-recent-file
    ;; preview-key (kdb "M-.")
    :preview-key '(:debounce 0.2 any)
    )

  ;; Optionally configure the narrowing key.
  ;; Both < and C++ work resonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (setq consult-line-numbers-widen t
    consult-async-min-input 2
    consult-async-refresh-delay 0.15
    consult-async-input-throttle 0.2
    consult-async-input-debounce 0.1)

  ;; Optinally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (autoload 'projectfile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(defun down-from-outside ()
  "Move to next candidate in minibuffer, even when minibuffer isn't selected."
  (interactive)
  (with-selected-window (active-minibuffer-window)
    (execute-kbd-macro [down])))

(defun up-from-outside ()
  "Move to previous candidate in minibuffer, even when minibuffer isn't selected."
  (interactive)
  (with-selected-window (active-minibuffer-window)
    (execute-kbd-macro [up])))

(defun to-and-fro-minibuffer ()
  "Go back and forth between minibuffer and other window."
  (interactive)
  (if (window-minibuffer-p (selected-window))
    (select-window (minibuffer-selected-window))
    (select-window (active-minibuffer-window))))

(global-set-key (kbd "s-i") #'to-and-fro-minibuffer)
(global-set-key (kbd "s-n") #'down-from-outside)
(global-set-key (kbd "s-p") #'up-from-outside)

(defun vertico-directory-delete-entry ()
  "Delete directory or entire entry before point."
  (interactive)
  (when (and (> (point) (minibuffer-prompt-end))
          ;; Check vertico--base for stepwise file path completion
          (not (equal vertico--base ""))
          (eq 'file (vertico--metadata-get 'category)))
    (save-excursion
      (goto-char (1- (point)))
      (when (search-backward "/" (minibuffer-prompt-end) t)
        (delete-region (1+ (point)) (point-max))
        t))))

(use-package consult-ag)

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
          :map minibuffer-local-completion-map
          ("C-x C-d" . consult-dir)
          ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-spotify
  :after consult)

(use-package fzf
  :if (executable-find "fzf")
  ;; :bind (("C-c f" . fzf))
  :config
  ;; (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
  (setq fzf/args "-x --print-query --margin=1,0 --no-hscroll"
    fzf/executable "fzf"
    fzf/git-grep-args "-i --line-number %s"
    ;; command used for `fzf-grep-*` functions
    ;; example usage for ripgrep:
    ;; fzf/grep-command "rg --no-heading -nH"
    fzf/grep-command "grep -nrH"
    ;; If nil, the fzf buffer will appear at the top of the window
    fzf/position-bottom t
    fzf/window-height 15))

(use-package flimenu
  :defer t
  :after imenu
  :config (flimenu-global-mode 1))

(use-package imenu-list
  :defer t
  :after (imenu consult)
  :commands imenu-list-smart-toggle
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-after-jump-hook nil)
  (setq imenu-list-auto-resize t))

(use-package savehist
  :straight (:type built-in)
  :hook (after-init . savehist-mode)
  :init
  (setq-default prescient-history-length 1000)
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
    history-length 100
    savehist-additional-variables '(mark-ring
                                     global-mark-ring
                                     search-ring
                                     regexp-search-ring
                                     extended-command-history)
    savehist-autosave-interval 3000)
  (put 'minibuffer-history 'history-length 50)
  (put 'evil-ex-history 'history-length 50)
  (put 'kill-ring 'history-length 25))


(use-package vertico
  :straight (:includes (vertico-buffer
                         vertico-directory
                         vertico-reverse
                         vertico-flat
                         vertico-repeat
                         vertico-unobstrusive
                         vertico-grid
                         vertico-multiform)
              :files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  :custom
  (vertico-scroll-margin 0) ;; different scroll margin
  (vertico-resize t)
  (vertico-cycle t)
  :config
  (define-advice vertico--update (:after (&rest _) choose-candidate)
    "Pick the previous directory rather than the prompt after updating candidates."
    (cond
      (previous-directory ; select previous directory
        (setq vertico--index (or (seq-position vertico--candidates previous-directory)
                               vertico--index))
        (setq previous-directory nil))))

  (defvar previous-directory nil
    "The directory that was just left. It is set when leaving a directory and
    set back to nil once it is used in the parent directory.")

  (defun set-previous-directory ()
    "Set the directory that was just exited from within find-file."
    (when (> (minibuffer-prompt-end) (point))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (minibuffer-prompt-end) t)
          ;; set parent directory
          (setq previous-directory (buffer-substring (1+ (point)) (point-max)))
          ;; set back to nil if not sorting by directories or what was deleted is not a directory
          (when (not (string-suffix-p "/" previous-directory))
            (setq previous-directory nil))
          t))))
  (advice-add #'vertico-directory-up :before #'set-previous-directory))

;; repeat last vertico session
(use-package vertico-repeat
  :straight nil
  :after vertico
  :bind
  ("M-r" . vertico-repeat))


(use-package vertico-posframe
  :init (vertico-posframe-cleanup)
  :after (vertico posframe)
  :config (setq vertico-posframe-border-width 15))

;; Enable rich annotations in completion UI
(use-package marginalia
  :after vertico
  :config
  (setq marginalia-command-categories (append marginalia-command-categories
                                        '((projectile-find-file . project-file)
                                           (projectile-find-dir . project-file)
                                           (projectile-switch-project . file)
                                           (persp-switch-to-buffer . buffer)
                                           (flycheck-error-list-set-filter . builtin))))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (:map minibuffer-local-map ("M-a" . marginalia-cycle))
  :init (marginalia-mode))



(defun +embark-live-vertico()
  "Shrink vertico minibuffer when `embark-live' is active."
  (when-let (win (and (string-prefix-p "*Embark Live" (buffer-name))
                   (active-minibuffer-window)))
    (with-selected-window win
      (when (and (bound-and-true-p vertico--input)
              (fboundp 'vertico-multiform-unobtrusive))
        (vertico-multiform-unobtrusive)))))
(add-hook 'embark-collect-mode-hook #'+embark-live-vertico)


;; manual preview for non-consult commands using embark
(define-key minibuffer-local-map (kbd "M-.") #'my-embark-preview)
(defun my-embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))


;; (use-package orderless
;;   :init (icomplete-mode)
;;   (setq orderless-style-dispatchers '(+orderless-dispatch))
;;   :custom (completion-styles '(basic substring partial-completion flex orderless)))

(use-package consult-ls-git
  :bind (("C-c g f" . consult-ls-git)
          ("C-c g F" . consult-ls-git-other-window)))

(use-package all-the-icons-completion
  :after all-the-icons
  :config (all-the-icons-completion-mode)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))


(provide 'init-consult)
;;; init-consult.el ends here
