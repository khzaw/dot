;; Curly quotes when writing in markup languages
(use-package typo :defer t)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind ((:map markdown-mode-map
          ("C-c v" . markdown-view-mode))
         (:map markdown-view-mode-map
          ("C-c v" . markdown-mode)))
  :init (setq markdown-command "multimarkdown"))

(use-package impatient-mode)

(defun markdown-filter (buffer)
  (princ
    (with-temp-buffer
      (let ((tmp (buffer-name)))
        (set-buffer buffer)
        (set-buffer (markdown tmp))
        (format "<!DOCTYPE html><html><title>Markdown Preview</title><link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown.min.css\"/><body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width:200px; max-width:980px;margin: 0 auto; padding: 45px;\">%s</article></body></html>" (buffer-string))))))

(defun markdown-realtime-preview ()
  "Preview markdown"
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'markdown-filter)
  (imp-visit-buffer))


(provide 'init-markdown)
;;; init-markdown.el ends here
