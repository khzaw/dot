
;; -*- lexical-binding: t; -*-
(use-package yaml-pro
  :hook ((yaml-ts-mode . yaml-pro-ts-mode)
         (yaml-mode . yaml-pro-mode)))

(use-package major-mode-hydra
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
