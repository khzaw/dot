(use-package flycheck
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  ;; Small BitMap-Arrow
  ;;(global-flycheck-mode)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  ;; don't recheck on idle as often
  (setq flycheck-idle-change-delay 1.0)
  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25)


  :custom-face
  (flycheck-warning ((t (:underline (:color "#fabd2f" :style line :position line)))))
  (flycheck-error ((t (:underline (:color "#fb4934" :style line :position line)))))
  (flycheck-info ((t (:underline (:color "#83a598" :style line :position line)))))
  :delight " ∰")

(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  :config (setq flycheck-popup-tip-error-prefix "X "))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package flycheck-posframe
  :after (flycheck posframe)
  :hook (global-flycheck-mode . flycheck-posframe-mode)
  :init
  (setq flycheck-posframe-border-width 1)
  :config
  (flycheck-posframe-configure-pretty-defaults)
  (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)
  (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p)
  (setq flycheck-posframe-warning-prefix "! "
    flycheck-posframe-info-prefix "··· "
    flycheck-posframe-error-prefix "X ")
  )

(provide 'init-flycheck)
;;; init-flycheck.el ends here
