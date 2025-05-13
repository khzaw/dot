;; -*- lexical-binding: t; -*-

(use-package latex
  :straight auctex
  :bind
  (:map LaTeX-mode-map
        ([remap next-error])
        ([remap previous-error])
        ("M-g M-n" . TeX-next-error)
        ("M-g M-p" . TeX-previous-error))
  :custom
  (TeX-save-query nil)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (LaTeX-mode . make-backslash-a-prefix-in-LaTeX)
  (LaTeX-mode . prettify-symbols-mode)
  (LaTeX-mode . add-prettify-symbols-hook)
  :config
  (defun make-backslash-a-prefix-in-LaTeX ()
    "Set the syntax class of \\ to ' in LaTeX buffers."
    (modify-syntax-entry ?\\ "'" LaTeX-mode-syntax-table))
  (defun toggle-prettify-symbols ()
    "Toggle `prettify-symbols-mode'."
    (prettify-symbols-mode 'toggle))
  (defun add-prettify-symbols-hook ()
    "Add toggling `prettify-symbols-mode' to local value of `visible-mode-hook'."
    (add-hook 'visible-mode-hook 'toggle-prettify-symbols nil t)))

(use-package cdlatex
  :straight t
  :hook ((LaTeX-mode . turn-on-cdlatex)))

(provide 'init-latex)
