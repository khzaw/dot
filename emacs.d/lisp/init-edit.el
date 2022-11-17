;; Automatically reload files modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

(use-package avy
  :bind (("C-;"   . avy-goto-char)
          ("C-'"   . avy-goto-char-2)
          ("M-g g" . avy-goto-line)
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
  :hook ((clojure-mode . aggressive-indent-mode)
          (emacs-lisp-mode . aggressive-indent-mode)))

;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=55340
(defun fix-electric-indent ()
  "Honour `electric-pair-open-newline-between-pairs'.
  Member of `post-self-insert-hook' if `electric-pair-mode' is on."
  (when (and (if (functionp electric-pair-open-newline-between-pairs)
               (funcall electric-pair-open-newline-between-pairs)
               electric-pair-open-newline-between-pairs)
          (eq last-command-event ?\n)
          (< (1+ (point-min)) (point) (point-max))
          (eq (save-excursion
                (skip-chars-backward "\t\s")
                (char-before (1- (point))))
            (matching-paren (char-after))))
    (save-excursion (newline-and-indent))))
(advice-add 'electric-pair-open-newline-between-pairs-psif :override #'fix-electric-indent)

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode))

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


(use-package highlight-indent-guides
  :diminish
  :hook
  (yaml-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guites-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character))

(use-package undo-fu)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package yasnippet
  :hook ((text-mode
           prog-mode
           conf-mode
           snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets")
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package writeroom-mode
  :config
  (setq writeroom-width 100
    writeroom-fullscreen-effect nil
    writeroom-maximize-window nil))

(provide 'init-edit)
;;; init-edit.el ends here
