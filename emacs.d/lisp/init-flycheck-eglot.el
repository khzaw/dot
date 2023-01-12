(require 'flycheck)

(defvar-local +lsp--flycheck-eglot--current-errors nil)

(defun +lsp--flycheck-eglot-init (checker callback)
  "CHECKER is the checker (eglot).
CALLBACK is the functionthat we need to call when we are done, on all the errors."
  (eglot-flymake-backend #'+lsp--flycheck-eglot--on-diagnostics)
  (funcall callback 'finished +lsp--flycheck-eglot--current-errors))

(defun +lsp--flycheck-eglot--on-diagnostics (diags &rest _)
  (cl-labels
    ((flymake-diag->flycheck-err
       (diag)
       (with-current-buffer (flymake--diag-buffer diag)
         (flycheck-error-new-at-pos
           (flymake--diag-beg diag)
           (pcase (flymake--diag-type diag)
             ('eglot-note 'info)
             ('eglot-warning 'warning)
             ('eglot-error 'error)
             (_ (error "Unknown diagnostic type, %S" diag)))
           (flymake--diag-text diag)
           :end-pos (flymake--diag-end diag)
           :check 'eglot
           :buffer (current-buffer)
           :filename (buffer-file-name)))))
    (setq +lsp--flycheck-eglot--current-errors
      (mapcar #'flymake-diag->flycheck-err diags))
    ;; Call Flycheck to update the diagnostics corfu-annotations
    (flycheck-buffer-deferred)))

(defun +lsp--flycheck-eglot-available-p ()
  (bound-and-true-p eglot--managed-mode))

(flycheck-define-generic-checker 'eglot
  "Report `eglot' diagnostics using `flycheck'."
  :start #'+lsp--flycheck-eglot-init
  :predicate #'+lsp--flycheck-eglot-available-p
  :modes '(prog-mode text-mode))

(push 'eglot flycheck-checkers)

(add-hook 'eglot-managed-mode-hook
  (defun +lsp-eglot-prefer-flycheck-h ()
    (when eglot--managed-mode
      (flymake-mode -1)
      (when-let ((current-checker (flycheck-get-checker-for-buffer)))
        (unless (equal current-checker 'eglot)
          (flycheck-add-next-checker 'eglot current-checker)))
      (flycheck-add-mode 'eglot major-mode)
      (flycheck-mode 1)
      ;; Call flycheck on initialization to make sure to display initial
      ;; errors
      (flycheck-buffer-deferred))))

(with-eval-after-load
  (when (and
          (not (fboundp 'flymake--diag-buffer))
          (fboundp 'flymake--diag-locus))
    (defalias 'flymake--diag-buffer 'flymake--diag-locus)))

(provide 'init-flycheck-eglot.el)
