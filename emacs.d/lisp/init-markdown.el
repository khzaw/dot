;; -*- lexical-binding: t; -*-

;; Curly quotes when writing in markup languages
(use-package typo :defer t)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :preface
  ;; Some packages load `markdown-mode' indirectly (e.g. Forge) before we ever
  ;; visit a Markdown buffer.  Keep this bound so those early loads never trip
  ;; over an unbound variable during Custom initialization.
  (defvar markdown-code-lang-modes nil)

  (defconst khz/markdown-code-lang-modes-extra
    '(("json" . json-ts-mode)
      ("tsx" . tsx-ts-mode)
      ("typescript" . typescript-ts-mode)
      ("ts" . typescript-ts-mode)
      ("javascript" . js-ts-mode)
      ("js" . js-ts-mode)
      ("python" . python-ts-mode)
      ("py" . python-ts-mode)
      ("go" . go-ts-mode)
      ("golang" . go-ts-mode)
      ("rust" . rust-ts-mode)
      ("dockerfile" . dockerfile-ts-mode)
      ("diff" . diff-mode)
      ("shell" . sh-mode)
      ("zsh" . sh-mode))
    "Extra fenced-code language mappings for `markdown-mode'.")

  (defun khz/markdown-ts-ready-p ()
    "Return non-nil when the built-in Markdown tree-sitter mode is usable."
    (and (fboundp 'treesit-ready-p)
         (treesit-ready-p '(markdown markdown-inline) t)))

  (defun khz/markdown-fallback-mode ()
    "Use the best non-tree-sitter Markdown mode for the current buffer."
    (if (and buffer-file-name
             (string-match-p "\\(?:README\\.md\\|\\.mdx?\\)\\'" buffer-file-name))
        (gfm-mode)
      (markdown-mode)))

  (defun khz/markdown-mode ()
    "Use `markdown-ts-mode' when ready, otherwise avoid its text-mode fallback."
    (interactive)
    (if (khz/markdown-ts-ready-p)
        (markdown-ts-mode)
      (khz/markdown-fallback-mode)))

  (defun khz/markdown-visual-setup ()
    "Apply the shared visual setup for Markdown buffers."
    (word-wrap-whitespace-mode 1)
    (visual-line-mode 1)
    (mixed-pitch-mode 1)
    (outline-minor-mode 1))

  (defun khz/markdown-outline-setup ()
    "Make `outline-minor-mode' follow Markdown headings."
    (setq-local outline-regexp markdown-regex-header)
    (setq-local outline-level #'markdown-outline-level))

  (defun khz/markdown-ts-sync-faces ()
    "Make `markdown-ts-mode' reuse the configured Markdown faces."
    (set-face-attribute 'markdown-ts-delimiter nil :inherit 'markdown-markup-face)
    (set-face-attribute 'markdown-ts-heading-1 nil :inherit 'markdown-header-face-1)
    (set-face-attribute 'markdown-ts-heading-2 nil :inherit 'markdown-header-face-2)
    (set-face-attribute 'markdown-ts-heading-3 nil :inherit 'markdown-header-face-3)
    (set-face-attribute 'markdown-ts-heading-4 nil :inherit 'markdown-header-face-4)
    (set-face-attribute 'markdown-ts-heading-5 nil :inherit 'markdown-header-face-5)
    (set-face-attribute 'markdown-ts-heading-6 nil :inherit 'markdown-header-face-6)
    (set-face-attribute 'markdown-ts-list-marker nil :inherit 'markdown-list-face)
    (set-face-attribute 'markdown-ts-block-quote nil :inherit 'markdown-blockquote-face)
    (set-face-attribute 'markdown-ts-strikethrough nil :inherit 'markdown-strike-through-face)
    (set-face-attribute 'markdown-ts-language-keyword nil :inherit 'markdown-language-keyword-face)
    (set-face-attribute 'markdown-ts-task-unchecked nil :inherit 'markdown-gfm-checkbox-face)
    (set-face-attribute 'markdown-ts-task-checked nil :inherit 'markdown-gfm-checkbox-face)
    (set-face-attribute 'markdown-ts-code-span nil :inherit 'markdown-inline-code-face)
    (set-face-attribute 'markdown-ts-code-block nil :inherit 'markdown-code-face))

  (defun khz/markdown-ts-setup ()
    "Apply Markdown package visuals to `markdown-ts-mode'."
    (khz/markdown-visual-setup)
    (require 'markdown-mode nil t)
    (khz/markdown-ts-sync-faces))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . khz/markdown-mode)
         ("\\.mdx\\'" . khz/markdown-mode))
     :bind ((:map markdown-mode-map
          ("C-c v"   . markdown-view-mode)
          ("M-p"     . markdown-outline-previous)
          ("M-n"     . markdown-outline-next)
          ("C-c C-f" . markdown-follow-thing-at-point)
          ("C-c C-n" . markdown-outline-next-same-level)
          ("C-c C-p" . markdown-outline-previous-same-level)
          ("C-c C-u" . markdown-outline-up))
         (:map markdown-view-mode-map
          ("C-c v" . markdown-mode)))
  :custom
  (markdown-command "multimarkdown")
  ;; Typography
  (markdown-header-scaling t)
  (markdown-header-scaling-values '(2.0 1.7 1.4 1.1 1.0 1.0))
  (markdown-enable-highlighting-syntax t)
  (markdown-enable-math t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-markup nil)
  ;; Images
  (markdown-max-image-size '(800 . 600))
  ;; List behavior
  (markdown-list-indent-width 2)
  (markdown-indent-on-enter 'indent-and-new-item)
  ;; GFM extensions
  (markdown-gfm-use-electric-backquote nil)
  :custom-face
  (markdown-bold-face ((t (:weight bold))))
  (markdown-header-face-1 ((t (:inherit markdown-header-face :weight bold))))
  (markdown-header-face-2 ((t (:inherit markdown-header-face :weight bold))))
  :hook ((markdown-mode . khz/markdown-visual-setup)
         (markdown-mode . khz/markdown-outline-setup)
         (gfm-mode . khz/markdown-visual-setup)
         (gfm-mode . khz/markdown-outline-setup))
  :config
  (dolist (entry (reverse khz/markdown-code-lang-modes-extra))
    (add-to-list 'markdown-code-lang-modes entry))
  (setq markdown-split-window-direction 'right))

(use-package markdown-toc
  :commands (markdown-toc-generate-toc
             markdown-toc-refresh-toc
             markdown-toc-generate-or-refresh-toc
             markdown-toc-delete-toc))

(use-package markdown-table-wrap
  :straight (:type git :host github :repo "dnouri/markdown-table-wrap")
  :defer t)

(use-package impatient-mode
  :commands (impatient-mode khz/markdown-realtime-preview)
  :preface
  (defun khz/markdown-filter (buffer)
    (princ
     (with-temp-buffer
       (let ((tmp (buffer-name)))
         (set-buffer buffer)
         (set-buffer (markdown tmp))
         (format "<!DOCTYPE html><html><title>Markdown Preview</title><link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.8.1/github-markdown.min.css\"/><body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width:200px; max-width:980px;margin: 0 auto; padding: 45px;\">%s</article></body></html>" (buffer-string))))))

  (defun khz/markdown-realtime-preview ()
    "Preview markdown."
    (interactive)
    (unless (process-status "httpd")
      (httpd-start))
    (impatient-mode)
    (imp-set-user-filter 'khz/markdown-filter)
    (imp-visit-buffer)))

(use-package markdown-xwidget
  :after markdown-mode
  :if (featurep 'xwidget-internal)
  :straight (markdown-xwidget
             :type git
             :host github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources"))
  :bind (:map markdown-mode-command-map
              ("x" . markdown-xwidget-preview-mode)))

(use-package markdown-ts-mode
  :straight nil
  :commands (markdown-ts-mode markdown-ts-view-mode)
  :hook (markdown-ts-mode . khz/markdown-ts-setup)
  :bind ((:map markdown-ts-mode-map
          ("C-c v"   . markdown-ts-view-mode)
          ("M-p"     . outline-previous-heading)
          ("M-n"     . outline-next-heading)
          ("C-c C-n" . outline-next-heading)
          ("C-c C-p" . outline-previous-heading)
          ("C-c C-u" . outline-up-heading))
         (:map markdown-ts-view-mode-map
          ("C-c v" . markdown-ts-mode)))
  :config
  (require 'markdown-mode nil t)
  (setq markdown-ts-hide-markup markdown-hide-markup
        markdown-ts-fontify-code-blocks-natively markdown-fontify-code-blocks-natively)
  (dolist (entry (reverse khz/markdown-code-lang-modes-extra))
    (add-to-list 'markdown-ts-code-block-modes
                 (list (intern (car entry)) (cdr entry)))))

(use-package md-ts-mode
  :straight (:type git :host github :repo "dnouri/md-ts-mode")
  :defer t)

(provide 'init-markdown)
;;; init-markdown.el ends here
