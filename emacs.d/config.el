(evil-mode 1)
(evil-ex-define-cmd "q" 'kill-this-buffer)  ; prevent accidentally killing the frame

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(defun khzaw/load-init()
  "Reload .emacs.d/init.el"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(smooth-scrolling-mode 1)
(setq scroll-margin 1
      smooth-scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

(setq initial-frame-alist (quote ((fullscreen . maximized))))

(set-frame-font "IosevkaCC-17" nil t)

(setq inhibit-splash-screen t)
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode t)
(setq blink-cursor-blinks 0) ;; blink forever
(setq-default indicate-empty-lines t)
(setq-default line-spacing 3)
(setq frame-title-format '("Emacs"))

(setq make-backup-files nil)

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(defun web-mode-init-hook ()
  "Hooks for web mode. Adjust indent."
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook 'web-mode-init-hook)

(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))
(add-hook 'web-mode-hook 'web-mode-init-hook)
