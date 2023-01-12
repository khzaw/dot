(use-package ispell
  :defer 15
  :config
  (progn
    (cond
      ((executable-find "aspell")
        (setq ispell-program-name "aspell")
        (setq ispell-extra-args '("--sug-mode=ultra"
                                   "--lang=en_US")))))
  (setq ispell-silently-savep t))

(use-package flymake-aspell :after flymake)

(provide 'init-spell)
