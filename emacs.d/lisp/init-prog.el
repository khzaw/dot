;; enable prettified symbols on comments
(defun setup-compose-predicate ()
  (setq prettify-symbols-compose-predicate
    (defun my-prettify-symbols-default-compose-p (start end _match)
      "Same as `prettify-symbols-default-compose-p', except compose symbols in comments as well."
      (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
                             '(?w ?_) '(?. ?\\)))
              (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
                              '(?w ?_) '(?. ?\\))))
        (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
               (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end)
               (nth 3 (syntax-ppss))))))))

(use-package prog-mode
  :straight (:type built-in)
  :commands (prettify-symbols-mode global-prettify-symbols-mode)
  :init
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  ;; :hook (prog-mode . prettify-hook)
  :config
  (global-prettify-symbols-mode))

(use-package hideshow
  :straight (:type built-in))
;; :hook (prog-mode . hs-minor-mode)
;; :bind (:map hs-minor-mode-map
;;         ("C-`" . hs-toggle-hiding)))

(use-package protobuf-mode)

(use-package yaml-pro
  :after (yaml-mode)
  :hook (yaml-mode . yaml-pro-mode))

(use-package terraform-mode)

(use-package csv-mode)

(use-package cask-mode)

(use-package lua-mode)

(use-package vimrc-mode)

(use-package sed-mode)

(use-package plantuml-mode
  :mode "\\.puml\\'"
  :config
  (setq homebrew-plantuml-jar-path
    (expand-file-name
      (string-trim (shell-command-to-string "brew list plantuml | grep jar"))))
  :custom
  (plantuml-jar-path homebrew-plantuml-jar-path)
  (plantuml-default-exec-mode 'executable))

(use-package graphviz-dot-mode
  :commands graphviz-dot-mode
  :mode ("\\.dot'" . graphviz-dot-mode))

(use-package ssh-config-mode)

(use-package swift-mode)

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

(use-package makefile-executor
  :hook (makefile-mode . makefile-executor-mode))

(advice-add 'risky-local-variable-p :override #'ignore)

(provide 'init-prog)
;;; init-prog.el ends here
