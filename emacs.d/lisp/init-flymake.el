(use-package quick-peek
  :commands (quick-peek-show))

(use-package flymake
  :straight (:type built-in)
  :bind (("C-c ! n" . flymake-goto-next-error)
          ("C-c ! p" . flymake-goto-prev-error)
          ("C-c ! c" . flymake-start))
  :config
  (setq flymake-suppress-zero-counters t)
  (setq flymake-fringe-indicator-position 'right-fringe)
  ;; (setq flymake-no-changes-timeout nil)
  (flymake-mode))

(use-package help-at-pt
  :init
  (setq help-at-pt-timer-delay 0.1)
  (setq help-at-pt-display-when-idle '(flymake-diagnostic)))

(use-package flymake-flycheck
  :disabled
  :config
  (setq-local flymake-diagnostic-functions
    (append flymake-diagnostic-functions (flymake-flycheck-all-chained-diagnostic-functions))))

(use-package flymake-diagnostic-at-point
  :ensure t
  :preface
  (defun flymake-diagnostic-at-point-quick-peek (text)
    "Display the flymake diagnostic TEXT with `quick-peek'`."
    (quick-peek-show (concat flymake-diagnostic-at-point-error-prefix text)))
  :hook
  (flymake-mode-hook . flymake-diagnostic-at-point-mode)
  :init
  (setq flymake-diagnostic-at-point-error-prefix nil))

(provide 'init-flymake)
