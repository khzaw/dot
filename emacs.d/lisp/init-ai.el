;; -*- lexical-binding: t; -*-

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-window-side 'right))

(use-package agent-shell
  :straight (:type git :host github :repo "xenodium/agent-shell"))

(use-package eca
  :straight (:type git :host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))

(provide 'init-ai)
