;; -*- lexical-binding: t; -*-

(use-package eldoc
  :straight t
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-display-truncation-message nil)
  :diminish
  :hook (ielm-mode . eldoc-mode)
  :config
  (eldoc-add-command-completions "paredit-")
  (eldoc-add-command-completions "combobulate-"))

(use-package xref
  :straight (:type built-in))

;; Run commands quickly
(use-package quickrun
  :bind (("C-<f5>" . quickrun)
         ("C-c X" . quickrun)))

(use-package prog-mode
  :straight (:type built-in)
  :custom
  (prettify-symbols-unprettify-at-point t)
  :hook (prog-mode . prettify-symbols-mode)
  :config (add-hook 'prog-mode-hook #'prettify-hook))

(use-package hideshow
  :straight (:type built-in))

(use-package protobuf-mode
  :hook (protobuf-mode . (lambda ()
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))


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

(use-package ssh-config-mode
  :straight (:type built-in)
  :mode ("jumphost*$'" . ssh-config-mode))

(use-package sh-mode
  :straight (:type built-in)
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.zsh\\'" . sh-mode)
         ("zshenv\\'" . sh-mode)
         ("zshenv.local\\'" . sh-mode)
         ("zshenv.linux\\'" . sh-mode)
         ("zshrc\\'" . sh-mode)
         ("zshenv.mac\\'" . sh-mode)))

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
  :if (executable-find "mmdc"))

(use-package makefile-executor
  :disabled t
  :hook (makefile-mode . makefile-executor-mode))

(advice-add 'risky-local-variable-p :override #'ignore)

(use-package mermaid-ts-mode
  :straight (:type git
                   :host github
                   :repo "d4ncer/mermaid-ts-mode"
                   :branch "main"
                   :files ("mermaid-ts-mode.el"))
  :mode (("\\.mermaid\\'" . mermaid-ts-mode))
  :config
  (defun khz/preview-mermaid ()
    "Render region inside a webit embebed browser."
    (interactive)
    (unless (region-active-p)
      (user-error "Select a region first"))
    (let* ((path (concat (make-temp-file (temporary-file-directory)) ".html"))
           (mermaid-code (buffer-substring-no-properties (region-beginning) (region-end))))
      (save-excursion
        (with-temp-buffer
          (insert "<body>
  <pre class=\"mermaid\">")
          (insert mermaid-code)
          ;; js script copied from mermaid documentation
          (insert "</pre>
  <script type=\"module\">
    import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';
    mermaid.initialize({ startOnLoad: true });
  </script>
</body>")
          (write-file path)))
      (xwidget-webkit-browse-url (format "file://%s" path)))))

(use-package sqlup-mode
  :straight (:type git :host github :repo "Trevoke/sqlup-mode.el")
  :hook ((sql-mode . sqlup-mode)
         (sql-interactive-mode . sqlup-mode)))

(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :hook (sql-mode . sqlformat-on-save-mode)
  :if (executable-find "pgformatter")
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-s2" "-g" "-u1")))

(use-package graphql-mode
  :straight (:type git :host github :repo "davazp/graphql-mode")
  :mode (("\\.graphqls\\'" . graphql-mode)
         ("\\.graphql\\'" . graphql-mode)))

(use-package graphviz-dot-mode
  :straight (:type git :host github :repo "ppareit/graphviz-dot-mode")
  :config
  (setq graphviz-dot-indent-width 2))

(use-package promql-mode
  :straight (:type git :host github :repo "Andor/promql-mode"))

(use-package dotenv-mode)

(use-package sicp)

(use-package anki-editor
  :straight (:type git :host github :repo "anki-editor/anki-editor"))

;; Code coverage in buffer
(use-package coverlay
  :commands (coverlay-load-file)
  :config
  (setq coverlay:tested-line-background-color "#c9f3d2")
  (setq coverlay:untested-line-background-color "#f8ced3")
  (setq coverlay:mark-tested-lines nil))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package jq-mode
  :straight (:type git :host github :repo "ljos/jq-mode")
  :mode "\\.jq\\'"
  :bind (:map json-ts-mode-map
              ("C-c C-j" . jq-interactively)))

(use-package edit-indirect
  :defer t)

(use-package zig-mode)

(use-package scala-ts-mode
  :straight (scala-ts-mode :type git :host github :repo "KaranAhlawat/scala-ts-mode"))

(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package structurizr-mode
  :straight (:type git :host github :repo "gilesp/structurizr-mode"))

(use-package d2-mode
  :if (executable-find "d2")
  :straight (:type git :host github :repo "andorsk/d2-mode")
  :mode "\\.d2\\'"
  :config
  (setq d2-output-format ".png")
  (setq d2-flags '("--sketch")))

(use-package kdl-mode
  :hook (kdl-mode . (lambda ()
                      (setq tab-width 4)
                      (setq indent-tabs-mode nil))))

(use-package hcl-mode
  :mode (("\\.hcl\\'" . hcl-mode)
         ("\\.tf\\'" . hcl-mode)
         ("\\.bru\\'" . hcl-mode)))

(use-package applescript-mode :mode "\\.applescript$")

(provide 'init-prog)
;;; init-prog.el ends here
