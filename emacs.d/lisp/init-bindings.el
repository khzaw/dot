;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Always newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

(defun khz/open-work-links ()
  "C-c e l Quickly open work bookmarks file"
  (interactive)
  (find-file-other-window "~/Library/CloudStorage/Dropbox/notes/fp/links.org"))

(global-set-key (kbd "C-c e l") 'khz/open-work-links)

;; M-return for frame maximization toggle
(global-set-key (kbd "<M-return>") 'toggle-frame-maximized)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<M-return>") 'toggle-frame-maximized))

;; Close frame if not the last, kill emacs else
(defun nano--delete-frame-or-kill-emacs ()
  "Delete frame or kill Emacs if there is only one frame."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-terminal)))
(global-set-key (kbd "C-x C-c") 'nano--delete-frame-or-kill-emacs)

;; Open recent files
(global-set-key (kbd "C-c r") 'recentf-open-files)

(provide 'init-bindings)
;;; init-bindings.el ends here
