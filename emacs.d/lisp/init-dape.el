(use-package dape
  :config
  (setq dape-buffer-window-arrangement 'left)
  (setq dape-inlay-hints t) ;; show inlay hints
  ;; kill compile buffers on build success
  (add-hook 'dape-compile-hook 'kill-buffer)
  (setq dape-cwd-fn 'projectile-project-root))


(provide 'init-dape)
