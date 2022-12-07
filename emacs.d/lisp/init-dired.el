(use-package dired
  :straight (:type built-in)
  :defer t
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-dwim-target t)
  :config
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq dired-use-ls-dired nil)
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))


  (add-hook 'dired-mode-hook
    (lambda()
      (define-key dired-mode-map (kbd "C-o") 'dired-view-current)
      (define-key dired-mode-map (kbd "n") 'dired-view-next)
      (define-key dired-mode-map (kbd "p") 'dired-view-previous)))

  (defun dired-view-next ()
    "Move down one line and view the current file in another window."
    (interactive)
    (dired-next-line)
    (dired-view-current))

  (defun dired-view-previous ()
    "Move up one line and view the current file in another window."
    (interactive)
    (dired-previous-line)
    (dired-view-current))

  (defun dired-view-current ()
    "View current file in another window (possibly newly created)."
    (interactive)
    (if (not (window-parent))
      (split-window)) ; create a new window if necessary
    (let ((file (dired-get-file-for-visit))
           (dbuffer (current-buffer)))
      (other-window 1) ; switch to the other window
      (unless (equal dbuffer (current-buffer)) ; don't kill the dired buffer
        (if (or view-mode (equal major-mode 'dired-mode)) ; only if in-view or dired-mode
          (kill-buffer))) ; kill it
      (let ((filebuffer (get-file-buffer file)))
        (if filebuffer  ; does a buffer already look at the file
          (switch-to-buffer filebuffer) ; simply switch
          (view-file file))   ; view it
        (other-window -1))))  ; give the attention back to the dired buffer


  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
            (")" . dired-git-info-mode)))

  (use-package diredfl
    :init (diredfl-global-mode 1))

  (use-package dired-sidebar))



(provide 'init-dired)
;;; init-dired.el ends here
