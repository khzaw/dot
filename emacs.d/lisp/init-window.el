;; -*- lexical-binding: t; -*-
(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-char-position 'left
              aw-ignore-current nil
              aw-leading-char-style 'char
              aw-scope 'global)
  :bind (("M-o" . ace-window)
         ("M-O" . ace-swap-window)))

(use-package shackle
  :init
  (setq shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-inhibit-window-quit-on-same-windows t
        shackle-rules '((help-mode :align below :size 0.3 :select t :inhibit-window-quit nil)
                        (helpful-mode :align below :inhibit-window-quit nil)
                        (devdocs-mode :align below :size 0.3 :select t :inhibit-window-quit nil)
                        (compilation-mode :select nil :size 0.25)
                        ("*ag search*" :select nil :size 0.25)
                        ("*Warnings*" :select nil :size 0.25)
                        ("*Error*" :select nil :size 0.25)
                        ("*Org-Babel Error Output*" :select nil :size 0.25)
                        (flymake-diagnostics-buffer-mode :align below :inhibit-window-quit nil)
                        (flycheck-error-message-mode :align below :inhibit-window-quit nil)
                        (flycheck-error-list-mode :align below :inhibit-window-quit nil)
                        (inferior-python-mode :align below :size 0.4 :select t)
                        (py-shell-mode :align below :size 0.4 :select t)
                        ;; (magit-status-mode :quit t :align bottom :size 0.5 :select t)
                        ;; (magit-revision-mode :same t :inhibit-window-quit t)
                        ;; (magit-log-mode :same t :inhibit-window-quit t)
                        ;; (magit-diff-mode :same t :inhibit-window-quit t)
                        ;; (magit-stash-mode :same t :inhibit-window-quit t)
                        ;; (magit-log-mode :same t :inhibit-window-quit t)
                        ;; (magit-diff-mode :same t  :inhibit-window-quit t)
                        ;; (forge-pullreq-mode :align t :size 0.7 :same t :inhibit-window-quit t)
                        ("work.org" :align bottom :size 0.25 :same t :inhibit-window-quit t)
                        ("*eldoc*" :align bottom :size 0.4)
                        ;; (git-commit-mode :same t)
                        ;; (forge-post-mode :same t :inhibit-window-quit t)
                        ;; (forge-topic-list-mode :same t :inhibit-window-quit t)
                        (vc-annotate-mode :same t)))
  :config
  (shackle-mode 1))

(use-package popper
  :bind (("C-\\" . popper-toggle)
         ("M-\\" . popper-cycle)
         ("C-M-\\" . popper-toggle-type))
  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  :custom
  (popper-reference-buffers
   '(Custom-mode
     compilation-mode
     messages-mode
     help-mode
     helpful-mode
     occur-mode
     devdocs-mode
     utop-mode
     vterm-mode
     term-mode
     flymake-diagnostics-buffer-mode
     xref--xref-buffer-mode
     "\\*evil-registers\\*"
     "^\\*Apropos"
     "\\*Messages\\*"
     "^\\*Compile-Log\\*"
     "[Oo]utput\\*$"
     "\\*Shell Command Output\\*"
     "\\*Org-Babel Error Output\\*"
     "\\*Async Shell Command\\*"
     "\\*Completions\\*"
     "\\*Warnings\\*"
     "\\*eldoc\\*"
     "\\*Flymake log\\*"
     "\\*xref\\*"
     "^\\*ielm\\*"
     flycheck-error-list-mode
     flycheck-error-message-mode
     inferior-python-mode
     py-shell-mode
     "^\\*eshell.*\\*$" eshell-mode
     "^\\*term.*\\*$" term-mode
     "^\\*shell.*\\*$" shell-mode)))

(use-package winner-mode
  :straight (:type built-in)
  :after evil
  :hook (after-init . winner-mode)
  :bind
  (:map evil-window-map
   ("u" . winner-undo)
   ("U" . winner-redo)))

(use-package burly
  :straight (:type git :host github :repo "alphapapa/burly.el"))

(use-package zoom
  :bind (("C-c z z" . zoom))
  :config
  (setq zoom-size '(0.618 . 0.618)))

(use-package activities
  :straight (:type git :host github :repo "alphapapa/activities.el")
  :init
  (activities-mode)
  ;; (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

(use-package easysession
  :disabled t
  :diminish
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)

  :custom
  (easysession-mode-line-misc-info t)  ; Display the session in the modeline
  (easysession-save-interval (* 10 60))  ; Save every 10 minutes

  :init
  ;; Key mappings:
  ;; C-c l for switching sessions
  ;; and C-c s for saving the current session
  ;; (global-set-key (kbd "C-c l") 'easysession-switch-to)
  ;; (global-set-key (kbd "C-c s") 'easysession-save-as)

  ;; The depth 102 and 103 have been added to to `add-hook' to ensure that the
  ;; session is loaded after all other packages. (Using 103/102 is particularly
  ;; useful for those using minimal-emacs.d, where some optimizations restore
  ;; `file-name-handler-alist` at depth 101 during `emacs-startup-hook`.)
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103))

(use-package sinister
  :straight (sinister :type git
                      :host github
                      :repo "positron-solutions/sinister")
  :config
  (sinister-stillness-mode 1)
  (sinister-misc-settings))

(provide 'init-window)
;; init-window.el ends here
