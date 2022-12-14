(use-package prog-mode
  :straight (:type built-in)
  :commands (prettify-symbols-mode global-prettify-symbols-mode)
  :init
  (global-prettify-symbols-mode)
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (create-hook-helper prettify-symbols-prog ()
    ""
    :hooks (prog-mode-hook)
    (push '(">=" . ?≥) prettify-symbols-alist)
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '("!=" . ?≠) prettify-symbols-alist)
    (push '("&&" . ?∧) prettify-symbols-alist)
    (push '("||" . ?∨) prettify-symbols-alist)
    (push '("lambda" . ?λ) prettify-symbols-alist)
    (push '("=>" . ?⇒) prettify-symbols-alist)
    (push '("->" . ?→) prettify-symbols-alist)
    (push '("nil" . ?∅) prettify-symbols-alist))
  (create-hook-helper prettify-symbols-lisp ()
    ""
    :hooks (lisp-mode-hook)
    (push '("/=" . ?≠) prettify-symbols-alist)
    (push '("sqrt" . ?√) prettify-symbols-alist)
    (push '("not" . ?¬) prettify-symbols-alist)
    (push '("and" . ?∧) prettify-symbols-alist)
    (push '("or" . ?∨) prettify-symbols-alist))
  (create-hook-helper prettify-symbols-js ()
    ""
    :hooks (js2-mode-hook rjsx-mode-hook js-mode-hook typescript-mode-hook typescript-tsx-mode-hook)
    (push '("function" . ?λ) prettify-symbols-alist)
    (push '("null" . ?∅) prettify-symbols-alist))
  (create-hook-helper prettify-symbols-python ()
    ""
    :hooks (python-mode-hook)
    (push '("and" . ?∧) prettify-symbols-alist)
    (push '("or" . ?∨) prettify-symbols-alist)))

(use-package hideshow
  :straight (:type built-in)
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
          ("C-`" . hs-toggle-hiding)))

(use-package protobuf-mode)

(use-package yaml-mode)

(use-package terraform-mode)

(use-package csv-mode)

(use-package cask-mode)

(use-package lua-mode)

(use-package vimrc-mode)

(setq homebrew-plantuml-jar-path
  (expand-file-name
    (string-trim (shell-command-to-string "brew list plantuml | grep jar"))))
(use-package plantuml-mode
  :mode "\\.puml\\'"
  :custom
  (plantuml-jar-path homebrew-plantuml-jar-path)
  (plantuml-default-exec-mode 'executable))

(use-package graphviz-dot-mode
  :commands graphviz-dot-mode
  :mode ("\\.dot'" . graphviz-dot-mode))

(use-package ssh-config-mode)

(use-package vyper-mode)

(use-package conf-mode
  :straight (:type built-in)
  :mode (rx (or ".list"
              "CODEOWNERS"
              (and ".env" (* (and "." (+ word))))
              (and "." (+ word) "rc"))
          eos))

(use-package mermaid-mode
  :mode (("\\.mermaid\\'" . mermaid-mode)))


(advice-add 'risky-local-variable-p :override #'ignore)


(provide 'init-prog)
;;; init-prog.el ends here
