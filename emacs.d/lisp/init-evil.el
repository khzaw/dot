(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-minibuffer nil
        evil-undo-system 'undo-tree
        evil-mode-line-format nil
        evil-kill-on-visual-paste nil
        evil-symbol-word-search t
        evil-respect-visual-line-mode nil
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-want-C-u-scroll nil
        evil-ex-interactive-search-highlight 'selected-window
        evil-search-module 'evil-search
        evil-kbd-macro-suppress-motion-error t)
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
  ;; q is enough; ESC is way too easy to accidentally press
  (evil-define-key 'normal magit-status-mode-map [escape] nil)
  (evil-collection-init))

(use-package evil-leader
  :commands (evil-leader-mode)
  :init (global-evil-leader-mode)
  :config
  (progn
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "f" 'isearch-forward
      "F" 'isearch-backward
      "b" 'consult-buffer)))

(provide 'init-evil)
;;; init-evil.el ends here
