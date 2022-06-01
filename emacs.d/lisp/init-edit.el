;; Automatically reload files modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

(use-package avy
  :bind (("C-;"   . avy-goto-char)
          ("C-'"   . avy-goto-char-2)
          ("M-g f" . avy-goto-line)
          ("M-g w" . avy-goto-word-0)
          ("M-g e" . avy-goto-word-1))
  :hook (after-init . avy-setup-default)
  :config (setq avy-background t
            avy-all-windows nil
            avy-all-windows-alt t
            avy-style 'pre))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
          ;; WORKAROUND: Disable in big files due to the performance issues
          ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
          (find-file . (lambda ()
                         (if (> (buffer-size) (* 3000 80))
                           (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(gitconfig-mode asm-mode web-mode html-mode css-mode go-mode scala-mode prolog-inferior-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
    '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
            'java-mode 'go-mode 'swift-mode)
       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
               (thing-at-point 'line))))))


;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil))

;; Narrow/Widen
(use-package fancy-narrow
  :diminish
  :hook (after-init . fancy-narrow-mode))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
          (prog-mode . flyspell-prog-mode)
          (flyspell-mode . (lambda ()
                             (dolist (key '("C-;" "C-," "C-."))
                               (unbind-key key flyspell-mode-map)))))
  :init (setq flyspell-issue-message-flag nil
          ispell-program-name "aspell"
          ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :config
  ;; Correcting words with flyspell via Ivy
  (use-package flyspell-correct-ivy
    :after ivy
    :bind (:map flyspell-mode-map
            ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper))
    :init (setq flyspell-correct-interface #'flyspell-correct-ivy)))


(provide 'init-edit)
;;; init-edit.el ends here
