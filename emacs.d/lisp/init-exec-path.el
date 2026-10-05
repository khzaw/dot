;;; -*- lexical-binding: t -*-

;; Homebrew keeps emacsclient outside the copied Emacs.app. Select the client
;; for this Emacs major version before with-editor computes its default.
(when (eq system-type 'darwin)
  (let ((client
         (format "/opt/homebrew/opt/emacs-plus@%d/bin/emacsclient"
                 emacs-major-version)))
    (when (file-executable-p client)
      (setq with-editor-emacsclient-executable client))))

(defun khz/existing-executable (&rest paths)
  "Return the first executable path from PATHS."
  (catch 'found
    (dolist (path paths)
      (when (file-executable-p path)
        (throw 'found path)))))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  ;; Emacs.app can inherit a stale Cellar path after Homebrew upgrades zsh.
  ;; Pick a stable shell before exec-path-from-shell computes its defaults.
  (when (eq system-type 'darwin)
    (let ((shell (or (khz/existing-executable "/opt/homebrew/bin/zsh"
                                              "/usr/local/bin/zsh"
                                              "/bin/zsh")
                     shell-file-name)))
      (setq exec-path-from-shell-shell-name shell)
      (setq-default shell-file-name shell)
      (setq explicit-shell-file-name shell)
      (setenv "SHELL" shell)))
  ;; Skip interactive shell startup (no .zshrc / oh-my-zsh / antigen). PATH and
  ;; friends are set in .zshenv / .zprofile, so a login-only shell is enough
  ;; and is ~5x faster (~160ms vs ~860ms here).
  (setq exec-path-from-shell-arguments '("-l"))
  ;; Only pull the variables we actually need. The default list is large and
  ;; each extra var costs a little extraction work.
  (setq exec-path-from-shell-variables
        '("PATH"
          "MANPATH"
          "GOPATH"
          "OPAM_LAST_ENV"
          "OPAM_SWITCH_PREFIX"
          "CAML_LD_LIBRARY_PATH"
          "OCAML_TOPLEVEL_PATH"
          ;; Make Emacs-launched agents and subprocesses use the same GitHub
          ;; credentials as login shells.
          "GITHUB_TOKEN"
          "GH_TOKEN"
          "GITHUB_ENTERPRISE_TOKEN"
          "GHE_TOKEN"))
  :config
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
        (when-let* ((libgccjit-lib (car (file-expand-wildcards
                                         (expand-file-name "/opt/homebrew/opt/libgccjit/lib/gcc/*")))))
          (setenv "LIBRARY_PATH" (mapconcat #'identity
                                            (delq nil (list (getenv "LIBRARY_PATH")
                                                            libgccjit-lib))
                                            ":")))

        ;; Only set after LIBRARY_PATH can find gcc libraries.
        (setq comp-deferred-compilation t)
        (setq native-comp-deferred-compilation t)
        (setq comp-speed 3))
    (message "Native comp is *not* available"))

  (when (eq system-type 'gnu/linux)
    (progn
      (exec-path-from-shell-copy-env "SSH_AGENT_PID")
      (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
