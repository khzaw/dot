;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Always newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Indentation based on the indentation of the previous non-blank line.
(setq-default indent-line-function #'indent-relative-first-indent-point)

;; In modes such as `text-mode', pressing Enter multiple times removes
;; the indentation. The following fixes the issue and ensures that text
;; is properly indented using `indent-relative' or
;; `indent-relative-first-indent-point'.
(setq-default indent-line-ignored-functions '())

(defun khz/open-work-links ()
  "C-c e l Quickly open work bookmarks file"
  (interactive)
  (find-file-other-window "~/Dropbox/notes/fp/links.org"))

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

(defun khz/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

  The generic `keyboard-quit' does not do the expected thing when
  the minibuffer is open.  Whereas we want it to close the
  minibuffer, even without explicitly focusing it.

  The DWIM behaviour of this command is as follows:

  - When the region is active, disable it.
  - When a minibuffer is open, but not focused, close the minibuffer.
  - When the Completions buffer is selected, close it.
  - In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (while (> (minibuffer-depth) 0)
      (abort-recursive-edit)))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'khz/keyboard-quit-dwim)

(provide 'init-bindings)
;;; init-bindings.el ends here
