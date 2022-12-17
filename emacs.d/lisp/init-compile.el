(use-package compile
  :config
  (setq compilation-scroll-output t)
  (setq compilation-ask-about-save nil)
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  :hook
  (compilation-mode . hide-mode-line-mode)
  (compilation-mode . whitespace-mode)
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

(provide 'init-compile)
