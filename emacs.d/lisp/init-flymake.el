;; -*- lexical-binding: t; -*-

(use-package flymake
  :after evil
  :straight (:type built-in)
  :bind (("C-c f c" . flymake-start)
         ("C-c f d" . flymake-show-buffer-diagnostics)
         :map flymake-mode-map
         ("C-c f n" . flymake-goto-next-error)
         ("C-c f p" . flymake-goto-prev-error))
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicator-position 'right-margin)
  (flymake-margin-indicators-string
     '((error "!»" compilation-error)
       (warning "»" compilation-warning)
       (note "»" compilation-info)))
  :config
  (add-hook 'find-file-hook 'flymake-find-file-hook)
  (add-hook 'flymake-diagnostics-buffer-mode-hook #'visual-line-mode)
  (setq flymake-show-diagnostics-at-end-of-line nil)
  (evil-set-command-property 'flymake-goto-next-error :jump t)
  (evil-set-command-property 'flymake-goto-prev-error :jump t))

(use-package flymake-diagnostic-at-point
  :after (flymake evil-leader)
  :preface
  (defun flymake-diagnostic-at-point-quick-peek (text)
    "Display the flymake diagnostic TEXT with `quick-peek'`."
    (quick-peek-show (concat flymake-diagnostic-at-point-error-prefix text)))
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config
  (setq flymake-diagnostic-at-point-error-prefix "! ")
  ;; (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer)
  (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup)
  (evil-leader/set-key "j" 'flymake-goto-next-error)
  (evil-leader/set-key "k" 'flymake-goto-prev-error))

(use-package flymake-collection
  :straight (:type git :host github :repo "mohkale/flymake-collection")
  :hook (after-init . flymake-collection-hook-setup))

(use-package flymake-quickdef
  :after flymake
  :config


  ;; https://github.com/rhysd/actionlint
  (flymake-quickdef-backend flymake-check-actionlint
                            :pre-let ((actionlint-exec (executable-find "actionlint")))
                            :pre-check (unless actionlint-exec (error "Cannot find actionlint executable"))
                            :write-type 'file
                            :proc-form (list actionlint-exec "-format" "{{range $err := .}}{{$err.Filepath}}:{{$err.Line}}:{{$err.Column}}:{{$err.Message}}\n{{end}}" fmqd-temp-file)
                            :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\)$"
                            :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                                                    (col (string-to-number (match-string 3)))
                                                    (text (match-string 4))
                                                    (pos (flymake-diag-region fmqd-source lnum col))
                                                    (beg (car pos))
                                                    (end (cdr pos))
                                                    (msg (format "actionlint> %s" text)))
                                               (list fmqd-source beg end :warning msg)))
  (add-hook 'yaml-mode-hook
            (lambda ()
              (if (string-match-p ".*\\.github/workflows/.*\\.ya?ml" (buffer-file-name))
                  (add-hook 'flymake-diagnostic-functions 'flymake-check-actionlint nil t))))

  ;; https://github.com/hadolint/hadolint
  (flymake-quickdef-backend flymake-hadolint
                            :pre-let ((hadolint-exec (executable-find "hadolint")))
                            :pre-check (unless hadolint-exec (error "Cannot find hadolint executable"))
                            :write-type 'file
                            :proc-form (list hadolint-exec "--no-color" fmqd-temp-file)
                            :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\) \\(.*\\)$"
                            :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                                                    (col 0)
                                                    (text (match-string 3))
                                                    (pos (flymake-diag-region fmqd-source lnum col))
                                                    (beg (car pos))
                                                    (end (cdr pos))
                                                    (msg (format "hadolint> %s" text)))
                                               (list fmqd-source beg end :warning msg)))
  (add-hook 'dockerfile-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-hadolint nil t))))


(use-package help-at-pt
  :init
  (setq help-at-pt-timer-delay 0.1)
  (setq help-at-pt-display-when-idle '(flymake-diagnostic)))

(use-package flymake-ruff
  :config
  (add-hook 'python-mode-hook 'ruff-format-on-save-mode)
  (add-hook 'python-ts-mode-hook 'ruff-format-on-save-mode))


(use-package flymake-json
  :straight (:type git :host github :repo "purcell/flymake-json")
  :if (executable-find "jsonlint")
  :hook ((json-ts-mode . flymake-json-maybe-load)))

;; (use-package flymake-popon
;;   :straight (:type git :host github :repo "doomelpa/flymake-popon")
;;   :hook (flymake-mode . flymake-popon-mode))

(provide 'init-flymake)
