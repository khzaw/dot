(use-package company
  :diminish
  :hook ((after-init . global-company-mode)
          (evil-normal-state . company-abort))
  :config
  (setq
    company-tooltip-align-annotations t
    company-tooltip-limit 12
    company-tooltip-minimum-width 40
    company-idle-delay 0.2
    company-echo-delay (if (display-graphic-p) nil 0)
    company-minimum-prefix-length 2
    company-icon-margin 3
    company-require-match nil
    company-show-numbers t
    company-dabbrev-ignore-case nil
    company-dabbrev-downcase nil
    company-global-modes '(not erc-mode message-mode help-mode
                            gud-mode eshell-mode shell-mode vterm-mode)
    company-backends '((company-capf :with company-yasnippet)
                        (company-dabbrev-code company-keywords company-files)
                        company-yasnippet
                        company-restclient
                        company-dabbrev))
  :bind (
          (:map company-search-map
            ("C-n" . company-select-next)
            ("C-p" . company-select-previous)
            ("C-t" . company-search-toggle-filtering))
          (:map company-active-map
            ("TAB" . company-complete-selection)
            ("C-n" . company-select-next)
            ("C-p" . company-select-previous))
          )
  )

(use-package company-restclient :after company)

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
  :after company
  :config (company-prescient-mode))

(provide 'init-company)
;;; init-company.el ends here
