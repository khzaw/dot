(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :init
  (setq company-tooltip-align-annotations t
    company-tooltip-limit 12
    company-idle-delay 0
    company-echo-delay (if (display-graphic-p) nil 0)
    company-minimum-prefix-length 1
    company-icon-margin 3
    company-require-match nil
    company-dabbrev-ignore-case nil
    company-dabbrev-downcase nil
    company-global-modes '(not erc-mode message-mode help-mode
                            gud-mode eshell-mode shell-mode vterm-mode)
    company-backends '((company-capf :with company-yasnippet)
                        (company-dabbrev-code company-keywords company-files)
                        company-dabbrev))
  :bind
  (:map company-search-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-t" . company-search-toggle-filtering)
    :map company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)))

(use-package company-posframe
  :after (company posframe)
  :config (company-posframe-mode))

(use-package company-quickhelp
  :after company
  :config (company-quickhelp-mode 1))

;; Better sorting
(use-package prescient
  :commands prescient-persist-mode
  :init (prescient-persist-mode 1))

(use-package company-prescient
  :init (company-prescient-mode 1))

(provide 'init-company)
;;; init-company.el ends here
