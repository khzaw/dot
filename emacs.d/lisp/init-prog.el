(use-package prog-mode
  :straight (:type built-in)
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  :config
  (global-prettify-symbols-mode t))

(use-package hideshow
  :straight (:type built-in)
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
          ("C-`" . hs-toggle-hiding)))

(use-package protobuf-mode)

(use-package yaml-mode)

(use-package terraform-mode)

(use-package csv-mode)

(use-package cask-mode)

(use-package lua-mode)

(use-package vimrc-mode)

(use-package plantuml-mode
  :mode "\\.puml\\'"
  :custom
  (plantuml-jar-path "/usr/local/bin/plantuml")
  (plantuml-default-exec-mode 'executable))

(use-package graphviz-dot-mode
  :commands graphviz-dot-mode
  :mode ("\\.dot'" . graphviz-dot-mode))

(use-package ssh-config-mode)

(use-package vyper-mode)

(use-package conf-mode
  :straight (:type built-in)
  :mode (rx (or ".list"
              "CODEOWNERS"
              (and ".env" (* (and "." (+ word))))
              (and "." (+ word) "rc"))
          eos))

;; Enable colors in *compilation* buffer: https://stackoverflow.com/a/3072831/13215205
(defun colorize-compilation-buffer ()
  "Enable colors in the *compilation* buffer."
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(advice-add 'risky-local-variable-p :override #'ignore)

(provide 'init-prog)
;;; init-prog.el ends here
