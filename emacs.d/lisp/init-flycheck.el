;; -*- lexical-binding: t; -*-

(use-package flycheck
  :commands (flycheck-list-errors flycheck-buffer)
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  :hook (flycheck-mode . flycheck-set-indication-mode)
  :config
  (setq flycheck-indication-mode 'right-margin)
  ;; Small BitMap-Arrow
  ;;(global-flycheck-mode)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  ;; don't recheck on idle as often
  (setq flycheck-idle-change-delay 2.0)
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
  :bind (:map flycheck-error-list-mode-map
         ("C-n" . flycheck-error-list-next-error)
         ("C-p" . flycheck-error-list-previous-error)))

(use-package flycheck-popup-tip
  ;; :hook (flycheck-mode . flycheck-popup-tip-mode)
  )

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
        flycheck-posframe-error-prefix "X "))

(use-package flycheck-overlay
  :straight (flycheck-overlay :type git :host github :repo "konrad1977/flycheck-overlay")
  :hook (flycheck-mode . flycheck-overlay-mode)
  :config
  (setq flycheck-overlay-background-lightness 30)
  (setq flycheck-overlay-text-tint 'lighter)
  (setq flycheck-overlay-text-tint-percent 10)
  (setq flycheck-overlay-use-theme-colors t)
  (setq flycheck-overlay-debounce-interval 0.3))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
