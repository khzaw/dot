;; -*- lexical-binding: t; -*-

;; Curly quotes when writing in markup languages
(use-package typo :defer t)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
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
  :hook ((markdown-mode . word-wrap-whitespace-mode)
         (markdown-mode . visual-line-mode)
         (markdown-mode . mixed-pitch-mode)
         (markdown-mode . outline-minor-mode)
         (gfm-mode . word-wrap-whitespace-mode)
         (gfm-mode . visual-line-mode)
         (gfm-mode . mixed-pitch-mode)
         (gfm-mode . outline-minor-mode))
  :config
  (setq markdown-split-window-direction 'right)

  ;; Make outline-minor-mode follow markdown heading structure
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq-local outline-regexp markdown-regex-header)
              (setq-local outline-level #'markdown-outline-level))))

(use-package markdown-toc)

(use-package impatient-mode)

(defun markdown-filter (buffer)
  (princ
    (with-temp-buffer
      (let ((tmp (buffer-name)))
        (set-buffer buffer)
        (set-buffer (markdown tmp))
        (format "<!DOCTYPE html><html><title>Markdown Preview</title><link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown.min.css\"/><body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width:200px; max-width:980px;margin: 0 auto; padding: 45px;\">%s</article></body></html>" (buffer-string))))))

(defun markdown-realtime-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'markdown-filter)
  (imp-visit-buffer))


(provide 'init-markdown)
;;; init-markdown.el ends here
