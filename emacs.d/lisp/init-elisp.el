(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
          ("C-c C-x" . ielm)
          ("C-c C-c" . eval-defun)
          ("C-c C-b" . eval-buffer))
  :hook (emacs-lisp-mode . (lambda ()
                             "Disable the checkdoc checker."
                             (setq-local flycheck-disabled-checkers
                               '(emacs-lisp-checkdoc))))
  :config
  (use-package highlight-defined
    :hook (emacs-lisp-mode . highlight-defined-mode)
    :init (setq highlight-defined-face-use-itself t)))

(use-package eldoc :ensure nil :diminish)

(provide 'init-elisp)
;;; init-elisp.el ends here
