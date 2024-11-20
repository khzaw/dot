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

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

(use-package tempel-collection)

(use-package orderless
  :config
  (setq completion-styles '(orderless flex))
  (setq completion-category-overrides '((file (styles  . (basic partial-completion)))
                                        (eglot (styles . (orderless flex)))
                                        (eglot-capf (styles . (orderless flex))))))


(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu" :files (:defaults "extensions/*"))
  :custom
  (corfu-preselect 'directory) ;; select the first candidate, except for directories
  (corfu-auto t)
  (corfu-separator ?_)         ;; Set to orderless separator, if not using space
  (corfu-auto-delay 0.35)
  (corfu-quit-no-match 'separator) ;; or t
  (corfu-auto-prefix 3)
  ;; :bind
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :config

  ;; disable corfu for org-tempo
  (defun khz/completion-at-point-org-tempo (orig-fun &rest :args)
    (unless (and (equal (char-before) ?<)
                 (derived-mode-p 'org-mode))
      (apply orig-fun args)))
  (advice-add 'completion-at-point :around #'khz/completion-at-point-org-tempo)

  ;; Use RET only in shell modes
  (keymap-set corfu-map "RET" `(menu-item "" nil :filter
                                          ,(lambda (&optional _)
                                             (and (derived-mode-p 'eshell-mode 'comint-mode)
                                                  #'corfu-send))))
  ;; Complete on punctuation
  ;; https://github.com/minad/corfu/wiki#tab-and-go-completion
  (dolist (c (list (cons "SPC" " ")
                   (cons "." ".")
                   (cons "," ",")
                   (cons ":" ":")
                   (cons ")" ")")
                   (cons "}" "}")
                   (cons "]" "]")))
    (define-key corfu-map (kbd (car c)) `(lambda ()
                                           (interactive)
                                           (corfu-insert)
                                           (insert ,(cdr c)))))


  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell.
The idea is to avoid pressing RET twice; see README at
https://github.com/minad/corfu."
    (cond
     ((and (derived-mode-p 'eshell-mode)
           (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)
           (fboundp 'comint-send-input))
      (comint-send-input))))
  (advice-add #'corfu-insert :after #'corfu-send-shell)

  :bind (:map corfu-map
         ("C-p" . corfu-previous)
         ("C-n" . corfu-next)
         ("M-m" . corfu-move-to-minibuffer)
         ("M-SPC" . corfu-insert-separator)
         ("C-g" . corfu-quit)
         ;; Free RET key for less instrusive behavior.
         ;; ("RET" . nil)
         )
  :init
  (global-corfu-mode))

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
  (corfu-popupinfo-delay '(0.4 . 0.2))
  (corfu-popupinfo-direction 'vertical)
  :custom-face
  (corfu-popupinfo ((t (:height 1.0))))
  :init (corfu-popupinfo-mode))

(use-package corfu-history
  :straight nil
  :commands (corfu-history-mode)
  :init (corfu-history-mode))

(use-package corfu-prescient
  :after (corfu prescient)
  :config (corfu-prescient-mode 1))

(use-package corfu-quick
  :straight nil
  :after corfu
  :bind (:map corfu-map
         ("C-q" . corfu-quick-insert)))

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
;; (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

(use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-TAB"   . completion-at-point) ;; capf
         ("C-c c t" . complete-tag)        ;; etags
         ("C-c c d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c c h" . cape-history)
         ("C-c c f" . cape-file)
         ("C-c c k" . cape-keyword)
         ("C-c c s" . cape-elisp-symbol)
         ("C-c c e" . cape-elisp-block)
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
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(provide 'init-corfu)
