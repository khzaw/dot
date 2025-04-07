;; -*- lexical-binding: t; -*-

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
  :hook ((prog-mode . (lambda () (yafolding-mode)))
         (prog-mode . prettify-hook))
  (global-prettify-symbols-mode))


(use-package hideshow
  :straight (:type built-in))
;; :hook (prog-mode . hs-minor-mode)
;; :bind (:map hs-minor-mode-map
;;         ("C-`" . hs-toggle-hiding)))

(use-package protobuf-mode)

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
  :disabled t
  :if (executable-find "mmdc")
  :mode (("\\.mermaid\\'" . mermaid-mode)))

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
  :init (setq sqlformat-command 'pgformatter
              sqlformat-args '("-s2" "-g" "-u1")))

(use-package graphql-mode
  :straight (:type git :host github :repo "davazp/graphql-mode"))

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

(provide 'init-prog)
;;; init-prog.el ends here
