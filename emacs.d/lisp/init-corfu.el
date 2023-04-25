(defun corfu-beginning-of-prompt ()
  "Move to end of completion input."
  (interactive)
  (corfu--goto -1)
  (goto-char (car completion-in-region--data)))

(defun corfu-end-of-prompt ()
  "Move to end of completion input."
  (interactive)
  (corfu--goto -1)
  (goto-char (cadr completion-in-region--data)))

(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
         completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
          ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
      (cons #'tempel-expand
        completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

(use-package orderless
  :init
  :custom
  (completion-styles '(orderless flex))
  (completion-category-overrides '((eglot (styles orderless)))))

(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu"
              :files (:defaults "extensions/*"))
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  (corfu-preselect-first t)
  (corfu-scroll-margin 5)
  :bind (:map corfu-map
          ("M-m" . corfu-move-to-minibuffer)
          ([remap move-beginning-of-line] . corfu-beginning-of-prompt)
          ([remap move-end-of-line] . corfu-end-of-prompt)))

;; show candidate doc in echo area
(use-package corfu-echo
  :straight nil
  :after corfu
  :commands (corfu-echo-mode)
  :init (corfu-echo-mode))

(use-package corfu-popupinfo-mode
  :straight nil
  :after corfu
  :commands corfu-popupinfo-mode
  :custom
  (corfu-popupinfo-delay 0)
  (corfu-popupinfo-direction 'vertical)
  :custom-face
  (corfu-popupinfo ((t (:height 1.0))))
  :init (corfu-popupinfo-mode))

(use-package corfu-history
  :straight nil
  :after corfu
  :demand
  :config
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
            (bound-and-true-p vertico--input))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
      corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

(use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind ( ("C-c c p" . completion-at-point) ;; capf
          ("C-c c t" . complete-tag)        ;; etags
          ("C-c c d" . cape-dabbrev)        ;; or dabbrev-completion
          ("C-c c h" . cape-history)
          ("C-c c f" . cape-file)
          ("C-c c k" . cape-keyword)
          ("C-c c s" . cape-symbol)
          ("C-c c a" . cape-abbrev)
          ("C-c c i" . cape-ispell)
          ("C-c c l" . cape-line)
          ("C-c c w" . cape-dict)
          ("C-c c \\" . cape-tex)
          ("C-c c _" . cape-tex)
          ("C-c c ^" . cape-tex)
          ("C-c c &" . cape-sgml)
          ("C-c c r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(provide 'init-corfu)
