(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-minibuffer nil)
  (setq evil-undo-system 'undo-tree)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-symbol-word-search t)
  (setq evil-respect-visual-line-mode nil)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'fundamental-mode 'emacs)
  (evil-set-initial-state 'chronos-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

;; (use-package evil-surround
;;   :after evil
;;   :config
;;   (global-evil-surround-mode 1))

(use-package evil-leader
  :commands (evil-leader-mode)
  :init (global-evil-leader-mode)
  :config
  (progn
    (evil-leader/set-leader ",")
    (evil-leader/set-key "f" 'isearch-forward)
    (evil-leader/set-key "b" 'consult-buffer)
    (evil-leader/set-key "gi" 'eglot-find-implementation)))

(use-package evil-escape
  :after (evil evil-collection)
  :init (setq-default evil-escape-key-sequence "kj")
  :config (evil-escape-mode 1))

(provide 'init-evil)
;;; init-evil.el ends here
