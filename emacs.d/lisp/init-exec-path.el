;;; -*- lexical-binding: t -*-
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  ;; (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize)
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (progn
        (message "Native comp is available")
        (when (eq system-type 'darwin)
          (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))
        (setq native-comp-async-report-warning-errors 'silent)
        ;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
        ;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
        ;; Append to path to give priority to values from exec-path-from-shell-initialize.
        (add-to-list 'exec-path (concat invocation-directory (file-name-as-directory "bin")) t)
        (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
                                       (when (getenv "LIBRARY_PATH") ":")
                                       ;; This is where homebrew puts libgccjit libraries
                                       (car (file-expand-wildcards
                                             (expand-file-name "/opt/homebrew/opt/libgccjit/lib/gcc/*")))))

        ;; Only set after LIBRARY_PATH can find gcc libraries.
        (setq comp-deferred-compilation t)
        (setq native-comp-deferred-compilation t)
        (setq comp-speed 3))
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
