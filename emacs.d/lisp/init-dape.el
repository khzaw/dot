;; -*- lexical-binding: t; -*-

(use-package dape
  :commands (dape)
  :preface
  (defun khz/eglot-dape-debug-at-point ()
    "Jump to definition and start Dape debugging."
    (interactive)
    (call-interactively #'eglot-find-definition)
    (dape))
  :init
  (with-eval-after-load 'eglot
    (bind-key "C-c e D" #'khz/eglot-dape-debug-at-point eglot-mode-map))
  :config
  (setq dape-buffer-window-arrangement 'left)
  (setq dape-inlay-hints t) ;; show inlay hints

  ;; To not display info and/or buffers on startup
  (remove-hook 'dape-stopped-hook 'dape-info)
  (remove-hook 'dape-start-hook 'dape-repl)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; kill compile buffers on build success
  (add-hook 'dape-compile-hook 'kill-buffer)
  (setq dape-cwd-fn 'projectile-project-root))


(provide 'init-dape)
