;; -*- lexical-binding: t; -*-

;; Adapted from James Cherti's terminal performance recommendations:
;; https://www.jamescherti.com/emacs-terminal-performance-vterm-eat-ansi-term-ghostel/
(defun khz/terminal-performance-setup ()
  "Reduce editing and redisplay overhead in terminal emulator buffers.
Keep the mode line, Evil navigation and text composition intact.  Do not
apply this to Eshell or Shell buffers, which support normal Emacs editing."
  (setq-local font-lock-defaults '(nil t))
  (setq-local scroll-conservatively most-positive-fixnum)
  (setq-local scroll-margin 0)
  (setq-local hscroll-margin 0)
  (setq-local auto-hscroll-mode nil)
  (setq-local truncate-lines t)
  (setq-local nobreak-char-display nil)
  ;; Terminal grids use left-to-right layout, not paragraph-based bidi.
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local bidi-inhibit-bpa t)
  ;; Ghostel owns its row geometry; do not override its spacing.
  (unless (derived-mode-p 'ghostel-mode)
    (setq-local line-spacing 0))
  (setq-local process-adaptive-read-buffering nil)
  ;; Do not lower the 4 MiB limit configured in init-eglot.el.
  (when (< read-process-output-max (* 1024 1024))
    (setq-local read-process-output-max (* 1024 1024)))
  (buffer-disable-undo)
  ;; Opt out even if global highlighting is enabled after this hook runs.
  (setq-local global-hl-line-mode nil)
  (dolist (mode '(electric-pair-local-mode
                  electric-indent-local-mode
                  show-paren-local-mode
                  display-line-numbers-mode
                  display-fill-column-indicator-mode
                  hl-line-mode))
    (when (fboundp mode)
      (funcall mode -1)))
  ;; Only disable active optional modes; do not autoload packages here.
  (dolist (mode '(flymake-mode flycheck-mode yas-minor-mode
                  company-mode corfu-mode evil-surround-mode
                  evil-snipe-local-mode))
    (when (and (boundp mode) (symbol-value mode) (fboundp mode))
      (funcall mode -1)))
  ;; Ghostel uses Eldoc for link targets and composition for TTY rendering.
  (unless (derived-mode-p 'ghostel-mode)
    (when (bound-and-true-p eldoc-mode)
      (eldoc-mode -1))))

(use-package term
  :straight nil
  :defer t
  :hook (term-mode . khz/terminal-performance-setup))

(use-package eshell
  :straight nil
  :defer t
  :config
  (defun khz/reset-scrolling-vars-for-term ()
    "Locally reset scrolling behavior in term-like buffers"
    (setq-local scroll-conservatively 0)
    (setq-local scroll-margin 0))
  (add-hook 'eshell-mode-hook #'khz/reset-scrolling-vars-for-term))

(use-package vterm
  :straight t
  :defer t
  :hook (vterm-mode . khz/terminal-performance-setup)
  :custom
  ;; Lower latency trades more frequent redraws for faster visible feedback.
  (vterm-timer-delay 0.01)
  (vterm-max-scrollback 500) ; lines, not bytes
  ;; Applies on the next module build; avoid Linux-only linker/CPU flags.
  (vterm-module-cmake-args "-DCMAKE_BUILD_TYPE=Release")
  :bind (:map vterm-mode-map
              ("s-t" . vterm) ; open up new tabs quickly
              ("C-\\" . popper-cycle)))

(use-package eshell-syntax-highlighting
  :after (esh-mode eshell)
  :defer t
  :config (eshell-syntax-highlighting-global-mode +1))

(use-package eat
  :disabled t
  :straight
  (:type git
   :host codeberg
   :repo "akib/emacs-eat"
   :files ("*.el" ("term" "term/*.el") "*.texi"
           "*.ti" ("terminfo/e" "terminfo/e/*")
           ("terminfo/65" "terminfo/65/*")
           ("integration" "integration/*")
           (:exclude ".dir-locals.el" "*-tests.el")))
  :hook (eat-mode . khz/terminal-performance-setup)
  :custom
  (eat-minimum-latency 0.007)
  (eat-maximum-latency 0.05)
  (eat-term-scrollback-size (* 64 1024)) ; characters
  (eat-enable-shell-prompt-annotation nil))


(use-package toggle-term
  :straight (toggle-term :type git :host github :repo "justinlime/toggle-term.el")
  ;; :bind (("M-o f" . toggle-term-find)
  ;;        ("M-o t" . toggle-term-term)
  ;;        ("M-o s" . toggle-term-shell)
  ;;        ("M-o e" . toggle-term-eshell)
  ;;        ("M-o i" . toggle-term-ielm)
  ;;        ("M-o o" . toggle-term-toggle))
  :config
  (setq toggle-term-size 25)
  (setq toggle-term-switch-upon-toggle t))

(use-package ghostel
  :straight (:type git :host github :repo "dakra/ghostel")
  :hook (ghostel-mode . khz/terminal-performance-setup)
  :custom
  (ghostel-timer-delay 0.01)
  (ghostel-max-scrollback (* 1024 1024)) ; bytes
  :bind (("C-x m" . ghostel)
         :map ghostel-mode-map
         ("C-s" . consult-line)
         ("C-k" . khz/ghostel-send-C-k-and-kill)
         ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl"))))
  :init
  (with-eval-after-load 'projectile
    (define-key projectile-command-map (kbd "m") #'ghostel-project)
    (define-key projectile-command-map (kbd "M") #'ghostel-project-list-buffers))
  :config
  (defun khz/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel. Like normal Emacs `C-k'. Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))

(use-package evil-ghostel
  :straight nil
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(use-package shell-pop
  :bind ("C-c t s" . shell-pop)
  :custom
  (shell-pop-universal-key "C-c t s")
  (shell-pop-window-position "bottom")
  (shell-pop-term-shell shell-file-name)
  (shell-pop-window-size 25)
  (shell-pop-autocd-to-working-dir nil)
  :config
  (setopt shell-pop-shell-type '("vterm" "*vterm*"
                                 (lambda ()
                                   (when (fboundp 'vterm)
                                     (let ((vterm-shell shell-pop-term-shell))
                                       (vterm)))))))

(provide 'init-terminal)
;;; init-terminal.el ends here
