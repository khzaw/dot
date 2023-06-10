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
  (setq evil-search-module 'evil-search)
  :config
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'fundamental-mode 'emacs)
  (evil-set-initial-state 'chronos-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-mode 1))

(use-package evil-escape
  :straight (evil-escape :type git :host github :repo "syl20bnr/evil-escape")
  :init
  (setq-default evil-escape-key-sequence "kj")
  :config
  (evil-escape-mode))

(use-package evil-collection
  :after evil
  :config
  ;; (setq evil-collection-outline-bind-tab-p t) ;; enable <tab>-based bindings in Outline mode.
  (evil-collection-init))

(use-package evil-leader
  :commands (evil-leader-mode)
  :init (global-evil-leader-mode)
  :config
  (progn
    (evil-leader/set-leader ",")
    (evil-leader/set-key "f" 'isearch-forward)
    (evil-leader/set-key "b" 'consult-buffer)))

(provide 'init-evil)
;;; init-evil.el ends here
