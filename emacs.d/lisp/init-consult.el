;; -*- lexical-binding: t; -*-

(use-package consult
  :after projectile
  :bind (("C-c h" . consult-history)
         ;; ("C-c i" . consult-imenu)
         ("C-c r". consult-recent-file)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-c l t" . consult-theme)
         ("C-c n a" . consult-org-agenda)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig . switch-to-buffer
         ;; b Buffers
         ;; SPC Hidden buffers
         ;; * Modified buffers
         ;; f Files (Requires recentf-mode)
         ;; r File registers
         ;; m Bookmarks
         ;; p Project
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-c p b" . consult-project-buffer)
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

  :config

  ;; exclude these directories from `consult-find'
  (setq consult-find-args "find . -not ( -wholename */.* -prune -o -name -node_modules -prune )")

  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  ;; Load the latest search again in `consult-line' when pressing C-s C-s
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
   :preview-key '(:debounce 0.2 any))


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

;; Narrowing which-key help without delay
(defun immediate-which-key-for-narrow (fun &rest args)
  (let* ((refresh t)
         (timer (and consult-narrow-key
                     (memq :narrow args)
                     (run-at-time 0.05 0.05
                                  (lambda ()
                                    (if (eq last-input-event (elt consult-narrow-key 0))
                                        (when refresh
                                          (setq refresh nil)
                                          (which-key--update))
                                      (setq refresh t)))))))
    (unwind-protect
        (apply fun args)
      (when timer
        (cancel-timer timer)))))
(advice-add #'consult--read :around #'immediate-which-key-for-narrow)

;; dogears
(defvar consult--source-dogears
  (list :name     "Dogears"
        :narrow   ?d
        :category 'dogears
        :items    (lambda ()
                    (mapcar
                     (lambda (place)
                       (propertize (dogears--format-record place)
                                   'consult--candidate place))
                     dogears-list))
        :action   (lambda (cand)
                    (dogears-go (get-text-property 0 'consult--candidate cand)))))

(defun consult-dogears ()
  (interactive)
  (consult--multi '(consult--source-dogears)))

(use-package consult-ag :after consult)

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-git-log-grep
  :custom (consult-git-log-grep-open-function #'magit-show-commit)
  :bind (("C-c g l" . consult-git-log-grep)))

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
  (setq enable-recursive-minibuffers t
        history-length 100
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        vertico-repeat-history
                                        extended-command-history)
        savehist-autosave-interval 3000)
  (put 'minibuffer-history 'history-length 50)
  (put 'evil-ex-history 'history-length 50)
  (put 'kill-ring 'history-length 25))

(use-package vertico
  :straight (vertico :type git :host github :repo "minad/vertico" :files (:defaults "extensions/*"))
  :hook (on-first-input . vertico-mode)
  :custom
  (vertico-scroll-margin 0) ;; different scroll margin
  (vertico-resize t)
  (vertico-cycle t))

(use-package vertico-mouse :after vertico :straight nil :config (vertico-mouse-mode))

(use-package vertico-directory
  :after vertico
  :straight nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :straight nil
  :after vertico
  :config

  ;; M-V -> vertico-multiform-vertical
  ;; M-G -> vertico-multiform-grid
  ;; M-F -> vertico-multiform-flat
  ;; M-R -> vertico-multiform-reverse
  ;; M-U -> vertico-multiform-unobtrusive

  (setq vertico-multiform-commands
    '((projectile-switch-project grid indexed)))

  (setq vertico-multiform-categories
    '((file grid)
       (jinx grid (vertico-grid-annotate . 20))))

  (vertico-multiform-mode 1))

;; repeat last vertico session
(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind
  ("M-r" . vertico-repeat))

;; Add avy style keys in vertico selections
(use-package vertico-quick
  :straight nil
  :after vertico
  :bind (:map vertico-map
         ("M-q" . vertico-quick-insert)
         ("C-q" . vertico-quick-exit)))

(use-package vertico-posframe
  :init (vertico-posframe-cleanup)
  :after (vertico posframe)
  :config
  (setq vertico-posframe-parameters `((left-fringe . 10)
                                      (right-fringe . 10))
        vertico-posframe-border-width 5))

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

;; Manual preview for non-Consult commands using Embark
(define-key minibuffer-local-map (kbd "M-.") #'my-embark-preview)
(defun my-embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))


(use-package consult-ls-git
  :straight (consult-ls-git :type git :host github :repo "rcj/consult-ls-git")
  :bind
  (("C-c g f" . #'consult-ls-git)
   ("C-c g F" . #'consult-ls-git-other-window)))

(use-package all-the-icons-completion
  :after all-the-icons
  :config (all-the-icons-completion-mode)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package affe
  :if (executable-find "rg")
  :after consult
  :config
  (consult-customize affe-grep :preview-key "M-.")
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

(use-package vertico-truncate
  :straight (vertico-truncate :type git :host github :repo "jdtsmith/vertico-truncate")
  :config (vertico-truncate-mode 1))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun +embark-live-vertico ()
    "Shrink Vertico minibuffer when `embark-live' is active."
    (when-let (win (and (string-prefix-p "*Embark Live" (buffer-name))
                        (active-minibuffer-window)))
      (with-selected-window win
        (when (and (bound-and-true-p vertico--input)
                   (fboundp 'vertico-multiform-unobtrusive))
          (vertico-multiform-unobtrusive)))))
  (add-hook 'embark-collect-mode-hook #'+embark-live-vertico)

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  ;; Automatically resize auto-updating Embark Collect buffers to fit their contents
  ;; produce something similar to (setq resize-mini-windows t) for the minibuffer
  (add-hook 'embark-collect-post-revert-hook
            (defun resize-embark-collect-window (&rest _)
              (when (memq embark-collect--kind '(:live :completions))
                (fit-window-to-buffer (get-buffer-window)
                                      (floor (frame-height) 2) 1))))

  ;; Make embark works well with `keycast'
  (defun store-action-key+cmd (cmd)
    (force-mode-line-update t)
    (setq this-command cmd
          keycast--this-command-keys (this-single-command-keys)
          keycast--this-command-desc cmd))

  (advice-add 'embark-keymap-prompter :filter-return #'store-action-key+cmd)

  ;; version of keycast--update that accepts (and ignores) parameters
  (defun force-keycast-update (&rest _) (keycast--update))

  (advice-add 'embark-act :before #'force-keycast-update))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-xref-stack
  :after consult
  :straight (:type git :host github :repo "brett-lempereur/consult-xref-stack" :branch "main")
  :bind (("C-," . consult-xref-stack-backward)))

(provide 'init-consult)
;;; init-consult.el ends here
