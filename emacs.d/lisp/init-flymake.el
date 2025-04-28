;; -*- lexical-binding: t; -*-

(use-package flymake
  :straight (:type built-in)
  :bind (("C-c f c" . flymake-start)
         ("C-c f d" . flymake-show-buffer-diagnostics)
         :map flymake-mode-map
         ("C-c f n" . flymake-goto-next-error)
         ("C-c f p" . flymake-goto-prev-error))
  :config
  (setq flymake-suppress-zero-counters t)
  (setq flymake-fringe-indicator-position 'right-fringe)
  (add-hook 'flymake-diagnostics-buffer-mode-hook #'visual-line-mode)
  ;; (setq flymake-no-changes-timeout nil)
  ;; (flymake-mode)
  )


(use-package help-at-pt
  :init
  (setq help-at-pt-timer-delay 0.1)
  (setq help-at-pt-display-when-idle '(flymake-diagnostic)))

(use-package flymake-flycheck
  :disabled
  :config
  (setq-local flymake-diagnostic-functions
    (append flymake-diagnostic-functions (flymake-flycheck-all-chained-diagnostic-functions))))

(use-package flymake-ruff
  :config
  (add-hook 'python-mode-hook 'ruff-format-on-save-mode)
  (add-hook 'python-ts-mode-hook 'ruff-format-on-save-mode))

(use-package flymake-diagnostic-at-point
  :after flymake
  :preface
  (defun flymake-diagnostic-at-point-quick-peek (text)
    "Display the flymake diagnostic TEXT with `quick-peek'`."
    (quick-peek-show (concat flymake-diagnostic-at-point-error-prefix text)))
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode)
  :config
  (setq flymake-diagnostic-at-point-error-prefix "! ")
  ;; (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer)
  (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup))

(use-package flymake-json
  :straight (:type git :host github :repo "purcell/flymake-json")
  :if (executable-find "jsonlint")
  :hook ((json-ts-mode . flymake-json-maybe-load)))

;; (use-package flymake-popon
;;   :straight (:type git :host github :repo "doomelpa/flymake-popon")
;;   :hook (flymake-mode . flymake-popon-mode))

(provide 'init-flymake)
