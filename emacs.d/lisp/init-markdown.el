;; -*- lexical-binding: t; -*-

;; Curly quotes when writing in markup languages
(use-package typo :defer t)

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

(defun khz/md-ts-sync-faces ()
  "Make `md-ts-mode' reuse the configured Markdown faces."
  (set-face-attribute 'md-ts-delimiter nil :inherit 'markdown-markup-face)
  (set-face-attribute 'md-ts-heading-1 nil :inherit 'markdown-header-face-1)
  (set-face-attribute 'md-ts-heading-2 nil :inherit 'markdown-header-face-2)
  (set-face-attribute 'md-ts-heading-3 nil :inherit 'markdown-header-face-3)
  (set-face-attribute 'md-ts-heading-4 nil :inherit 'markdown-header-face-4)
  (set-face-attribute 'md-ts-heading-5 nil :inherit 'markdown-header-face-5)
  (set-face-attribute 'md-ts-heading-6 nil :inherit 'markdown-header-face-6)
  (set-face-attribute 'md-ts-list-marker nil :inherit 'markdown-list-face)
  (set-face-attribute 'md-ts-block-quote nil :inherit 'markdown-blockquote-face)
  (set-face-attribute 'md-ts-strikethrough nil :inherit 'markdown-strike-through-face)
  (set-face-attribute 'md-ts-language-keyword nil :inherit 'markdown-language-keyword-face)
  (set-face-attribute 'md-ts-task-list-marker nil :inherit 'markdown-gfm-checkbox-face)
  (set-face-attribute 'md-ts-code nil
                      :inherit '(markdown-inline-code-face markdown-code-face)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  ;; :mode (("README\\.md\\'" . gfm-mode)
  ;;        ("\\.md\\'" . gfm-mode)
  ;;        ("\\.markdown\\'" . markdown-mode))
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
  ;; Extend defaults with tree-sitter modes and common aliases
  (markdown-code-lang-modes
   (append '(("json" . json-ts-mode)
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
           markdown-code-lang-modes))
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
  (markdown-code-face ((t (:inherit fixed-pitch :extend t))))
  (markdown-inline-code-face ((t (:inherit (fixed-pitch font-lock-constant-face)))))
  :hook ((markdown-mode . khz/markdown-visual-setup)
         (markdown-mode . khz/markdown-outline-setup)
         (gfm-mode . khz/markdown-visual-setup)
         (gfm-mode . khz/markdown-outline-setup))
  :config
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
  :commands (impatient-mode))

(defun markdown-filter (buffer)
  (princ
    (with-temp-buffer
      (let ((tmp (buffer-name)))
        (set-buffer buffer)
        (set-buffer (markdown tmp))
        (format "<!DOCTYPE html><html><title>Markdown Preview</title><link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.8.1/github-markdown.min.css\"/><body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width:200px; max-width:980px;margin: 0 auto; padding: 45px;\">%s</article></body></html>" (buffer-string))))))

(defun markdown-realtime-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'markdown-filter)
  (imp-visit-buffer))

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

(use-package md-ts-mode
  :straight (:type git :host github :repo "dnouri/md-ts-mode")
  :hook (md-ts-mode . khz/markdown-visual-setup)
  :config
  (setq md-ts-hide-markup markdown-hide-markup
        md-ts-heading-scaling markdown-header-scaling
        md-ts-heading-scaling-values markdown-header-scaling-values)
  (khz/md-ts-sync-faces)
  (md-ts-mode-enable-global))

(provide 'init-markdown)
;;; init-markdown.el ends here
