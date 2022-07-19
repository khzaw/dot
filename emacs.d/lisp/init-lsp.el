(use-package lsp-mode
  :diminish
  :commands (lsp-enable-which-key-integration
              lsp-format-buffer
              lsp-organize-imports)
  :hook ((go-mode . lsp-deferred)
          ;; (prog-mode . (lambda ()
          ;;                (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
          ;;                  (lsp-deferred))))
          ;; (markdown-mode . lsp-deferred)
          (lsp-mode . (lambda ()
                        (lsp-enable-which-key-integration)
                        (add-hook 'before-save-hook #'lsp-format-buffer t t)
                        (add-hook 'before-save-hook #'lsp-organize-imports t t))))
  :bind (:map lsp-mode-map
          ("C-c C-d" . lsp-describe-thing-at-point)
          ([remap xref-find-definitions] . lsp-find-definitions)
          ([remap xref-find-references] . lsp-find-references))
  :init (setq lsp-keymap-prefix "C-c l"
          lsp-keep-workspace-alive nil
          lsp-signature-auto-activate nil
          lsp-modeline-code-actions-enable nil
          lsp-modeline-diagnostics-enable nil
          lsp-modeline-workspace-status-enable nil
          lsp-headerline-breadcrumb-enable nil
          lsp-enable-file-watchers nil
          lsp-enable-folding nil
          lsp-enable-symbol-highlighting t
          lsp-enable-text-document-color nil
          lsp-enable-indentation t
          lsp-enable-on-type-formatting t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (("C-c u" . lsp-ui-imenu)
          :map lsp-ui-mode-map
          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-doc-delay 0.1)
  :init
  (setq lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                               ,(face-foreground 'font-lock-string-face)
                               ,(face-foreground 'font-lock-constant-face)
                               ,(face-foreground 'font-lock-variable-name-face))))


(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
        ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
        ("C-s-." . lsp-ivy-global-workspace-symbol)))

(use-package lsp-treemacs
  :after lsp-mode
  :init (lsp-treemacs-sync-mode 1))

(provide 'init-lsp)
;;; init-lsp.el ends here
