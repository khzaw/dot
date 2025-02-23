(use-package compile
  :config

  (defun khz/compile-mode-hook ()
    (hide-mode-line-mode)
    (setq truncate-lines nil) ;; automatically becomes buffer local
    (set (make-local-variable 'truncate-partial-width-windows) nil))

  (setq compilation-scroll-output t)
  (setq compilation-ask-about-save nil)
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  :hook
  (compilation-mode . khz/compile-mode-hook)
  (compilation-start . olivetti-mode))

;; Enable colors in *compilation* buffer: https://stackoverflow.com/a/3072831/13215205
(defun colorize-compilation-buffer ()
  "Enable colors in the *compilation* buffer."
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package recompile-on-save
  :commands (recompile-on-save))

(use-package compile-multi :straight t)

(use-package consult-compile-multi
  :straight t
  :after compile-multi
  :config (consult-compile-multi-mode))

(use-package compile-multi-all-the-icons
  :straight t
  :after all-the-icons-completion
  :after compile-multi)

(use-package compile-multi-embark
  :straight t
  :after embark
  :after compile-multi
  :config (compile-multi-embark-mode +1))



(provide 'init-compile)
