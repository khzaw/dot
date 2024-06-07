
(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-char-position 'left
              aw-ignore-current nil
              aw-leading-char-style 'char
              aw-scope 'frame)
  :bind (("M-o" . ace-window)
         ("M-O" . ace-swap-window)))

(use-package shackle
  :init
  (setq
   shackle-default-alignment 'below
   shackle-default-size 0.4
   shackle-inhibit-window-quit-on-same-windows t
   shackle-rules '((help-mode :align below :size 0.25 :select t)
                   (helpful-mode :align below)
                   (compilation-mode :select nil :size 0.25)
                   ("*ag search*" :select nil :size 0.25)
                   ("*Warnings*" :select nil :size 0.25)
                   ("*Error*" :select nil :size 0.25)
                   ;; (magit-status-mode :align bottom :size 0.85 :select t :inhibit-window-quit t)
                   ;; (magit-log-mode :same t :inhibit-window-quit t)
                   ;; (magit-diff-mode :same t  :inhibit-window-quit t)
                   (forge-pullreq-mode :align t :size 0.7 :same t :inhibit-window-quit t)
                   ("work.org" :align bottom :size 0.25 :same t :inhibit-window-quit t)
                   (git-commit-mode :same t)
                   (vc-annotate-mode :same t)
                   ("\\*magit.*?\\*\\'" :regexp t :align t :same t :inhibit-window-quit 5 :size 0.7)
                   ))
  :config
  (shackle-mode 1))

(use-package popper
  :bind (("C-\\" . popper-toggle-latest)
          ("M-\\" . popper-cycle)
          ("C-M-\\" . popper-toggle-type))
  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  :custom
  (popper-reference-buffers
    '("\\*Messages\\*"
       "Output\\*$"
       "\\*Async Shell Command\\*"
       xref--xref-buffer-mode
       help-mode
       helpful-mode
       "\\*Warnings\\*"
       "\\*eldoc\\*"
       "\\*Flymake log\\*"
       "\\*xref\\*"
       "\\*Go Test\\*"
       flycheck-error-list-mode
       compilation-mode
       go-test-mode
       "^\\*vterm.*\\*$" vterm-mode
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

(provide 'init-window)
;;; init-window.el ends here
