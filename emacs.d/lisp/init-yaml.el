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
  (evil-define-key 'insert yaml-pro-ts-mode-map
    (kbd "C-<return>") 'yaml-pro-ts-meta-return))

(use-package major-mode-hydra
  :disabled t
  :after yaml-pro
  :config
  (major-mode-hydra-define yaml-ts-mode (:foreign-keys run)
    ("Navigation"
     (("u" yaml-pro-ts-up-level "Up level" :color pink) ; C-c C-u
      ("J" yaml-pro-ts-next-subtree "Next subtree" :color pink) ; C-c C-n
      ("K" yaml-pro-ts-prev-subtree "Previous" :color pink)) ; C-c C-p
     "Editing"
     (("m" yaml-pro-ts-mark-subtree "Mark subtree")  ; C-c C-@
      ("x" yaml-pro-ts-kill-subtree "Kill subtree")  ; C-c C-x C-w
      ("p" yaml-pro-ts-paste-subtree "Paste subtree")) ; C-c C-x C-y
     "Insert"
     (("e" yaml-pro-edit-ts-scalar "Edit item") ; C-c '
      ("o" yaml-pro-ts-meta-return "New list item"))
     "Refactor"
     (("r" yaml-pro-ts-move-subtree-up "Raise subtree")
      ("t" yaml-pro-ts-move-subtree-down "Lower subtree")
      ("," combobulate-hydra/body ">>>"))
     "Documentation"
     (("d" hydra-devdocs/body "Devdocs")))))

(provide 'init-yaml)
