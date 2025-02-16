;; -*- lexical-binding: t; -*-

(use-package flymake
  :straight (:type built-in)
  :bind (("C-c f n" . flymake-goto-next-error)
         ("C-c f p" . flymake-goto-prev-error)
         ("C-c f c" . flymake-start))
  :config
  (setq flymake-suppress-zero-counters t)
  (setq flymake-fringe-indicator-position 'right-fringe)
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
  (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer))

(use-package flymake-json
  :straight (:type git :host github :repo "purcell/flymake-json")
  :if (executable-find "jsonlint")
  :hook ((json-ts-mode . flymake-json-maybe-load)))


(provide 'init-flymake)
