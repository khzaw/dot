(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize)
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (progn
        (message "Native comp is available")
        (add-to-list 'exec-path (concat invocation-directory "bin") t)
        (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
                                       (when (getenv "LIBRARY_PATH") ":")
                                       (car (file-expand-wildcards
                                             (expand-file-name "~/homebrew/opt/gcc/lib/gcc/*")))))
        (setq native-comp-deferred-compilation t))
    (message "Native comp is *not* available"))

  (when (eq system-type 'gnu/linux)
    (progn
      (exec-path-from-shell-copy-env "SSH_AGENT_PID")
      (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode +1)
  (auto-compile-on-save-mode +1))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
