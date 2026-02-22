;; -*- lexical-binding: t; -*-

(defun my-yaml-ts-mode-setup ()
  "Custom setup applied when `yaml-ts-mode` starts."
  ;; Let yaml-ts-mode (Tree-sitter) handle its own indentation logic.
  ;; indent-for-tab-command should delegate to the mode's indent-line-function.
  ;; Ensure no global setting is overriding yaml-ts-mode's indent-line-function.

  ;; treesit-indent has a bug a doesn't work well in yaml-ts-mode
  (setq-local indent-line-function #'yaml-indent-line)

  ;; Other mode-specific settings
  ;; (define-key evil-insert-state-local-map (kbd "<tab>") #'indent-for-tab-command)
  ;; e.g., (setq-local comment-line-break-function #'adaptive-fill-comment-line)
  )

(use-package yaml-ts-mode
  :straight (:type built-in)
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-ts-mode)
         ("kubeconfig\\'" . yaml-ts-mode))
  :hook (yaml-ts-mode . my-yaml-ts-mode-setup))

(use-package yaml-pro
  :hook (yaml-ts-mode . yaml-pro-mode)
  :hook (yaml-ts-mode . yaml-pro-ts-mode)
  :config
  ;; Evil bindings mirroring org-mode's M-{h,j,k,l} subtree paradigm
  (evil-define-key 'normal yaml-pro-ts-mode-map
    ;; Subtree motion: M-j/M-k to reorder, M-h/M-l to indent/dedent
    (kbd "M-k") 'yaml-pro-ts-move-subtree-up
    (kbd "M-j") 'yaml-pro-ts-move-subtree-down
    (kbd "M-h") 'yaml-pro-ts-unindent-subtree
    (kbd "M-l") 'yaml-pro-ts-indent-subtree
    ;; Navigation
    (kbd "C-c C-u") 'yaml-pro-ts-up-level
    (kbd "C-c C-n") 'yaml-pro-ts-next-subtree
    (kbd "C-c C-p") 'yaml-pro-ts-prev-subtree
    ;; Structural editing
    (kbd "C-c C-@") 'yaml-pro-ts-mark-subtree
    (kbd "C-c C-x C-w") 'yaml-pro-ts-kill-subtree
    (kbd "C-c C-x C-y") 'yaml-pro-ts-paste-subtree
    (kbd "C-c '") 'yaml-pro-edit-ts-scalar)
  (evil-define-key 'visual yaml-pro-ts-mode-map
    (kbd "M-k") 'yaml-pro-ts-move-subtree-up
    (kbd "M-j") 'yaml-pro-ts-move-subtree-down
    (kbd "M-h") 'yaml-pro-ts-unindent-subtree
    (kbd "M-l") 'yaml-pro-ts-indent-subtree)
  (evil-define-key 'insert yaml-pro-ts-mode-map
    (kbd "C-<return>") 'yaml-pro-ts-meta-return))

(provide 'init-yaml)
