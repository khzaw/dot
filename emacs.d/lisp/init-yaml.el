
;; -*- lexical-binding: t; -*-

(defun my-yaml-backtab ()
  "Decrease indentation of the current line using standard-indent."
  (interactive)
  (let ((offset (or (bound-and-true-p standard-indent) 2))) ; Default to 2 if standard-indent is nil
    (when (> (current-indentation) 0)
      ;; Ensure we don't de-indent beyond column 0
      (indent-rigidly (line-beginning-position) (line-end-position) (- (min offset (current-indentation)))))))

(defun my-yaml-ts-mode-setup ()
  "Custom setup applied when `yaml-ts-mode` starts."
  ;; Let yaml-ts-mode (Tree-sitter) handle its own indentation logic.
  ;; indent-for-tab-command should delegate to the mode's indent-line-function.
  ;; Ensure no global setting is overriding yaml-ts-mode's indent-line-function.

  ;; treesit-indent has a bug a doesn't work well in yaml-ts-mode
  (setq-local indent-line-function #'yaml-indent-line)

  ;; --- Keybindings for Evil Insert State ---
  ;; Bind TAB to indent using the mode's logic in insert mode
  (define-key evil-insert-state-local-map (kbd "<tab>") #'indent-for-tab-command)

  ;; Bind Shift-Tab to de-indent using our custom function in insert mode
  (define-key evil-insert-state-local-map (kbd "<backtab>") #'my-yaml-backtab)

  ;; Other mode-specific settings can go here if needed
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
