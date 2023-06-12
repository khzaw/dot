(use-package ispell
  :disabled t
  :defer 15
  :config
  (progn
    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args '("--sug-mode=ultra"
                                "--lang=en_US")))))
  (setq ispell-silently-savep t))

(use-package flymake-aspell :disabled t :after flymake)

(use-package jinx
  :if (executable-find "enchant-2")
  :after (vertico vertico-multiform)
  :straight (jinx :type git :host github :repo "minad/jinx" :files (:defaults "jinx-mod.c" "emacs-module.h"))
  :hook (on-first-buffer . global-jinx-mode)
  :bind (([remap ispell-word] . #'jinx-correct))
  :config
  (vertico-multiform-mode 1))

(provide 'init-spell)
