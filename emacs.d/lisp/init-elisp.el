;; -*- lexical-binding: t; -*-

(use-package paredit)

(use-package elisp-mode
  :straight (:type built-in)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-x" . ielm)
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer))
  :hook (emacs-lisp-mode . (lambda ()
                             "Disable the checkdoc checker."
                             (setq-local flycheck-disabled-checkers
                                         '(emacs-lisp-checkdoc))))
  :config
  (use-package highlight-defined
    :hook (emacs-lisp-mode . highlight-defined-mode)
    :init (setq highlight-defined-face-use-itself t))
  (with-no-warnings
    ;; Align indent keywords
    ;; @see https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
    (defun my-lisp-indent-function (indent-point state)
      "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
      (let ((normal-indent (current-column))
            (orig-point (point)))
        (goto-char (1+ (elt state 1)))
        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
        (cond
         ;; car of form doesn't seem to be a symbol, or is a keyword
         ((and (elt state 2)
               (or (not (looking-at "\\sw\\|\\s_"))
                   (looking-at ":")))
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
         ((and (save-excursion
                 (goto-char indent-point)
                 (skip-syntax-forward " ")
                 (not (looking-at ":")))
               (save-excursion
                 (goto-char orig-point)
                 (looking-at ":")))
          (save-excursion
            (goto-char (+ 2 (elt state 1)))
            (current-column)))
         (t
          (let ((function (buffer-substring (point)
                                            (progn (forward-sexp 1) (point))))
                method)
            (setq method (or (function-get (intern-soft function)
                                           'lisp-indent-function)
                             (get (intern-soft function) 'lisp-indent-hook)))
            (cond ((or (eq method 'defun)
                       (and (null method)
                            (length> function 3)
                            (string-match "\\`def" function)))
                   (lisp-indent-defform state indent-point))
                  ((integerp method)
                   (lisp-indent-specform method state
                                         indent-point normal-indent))
                  (method
                   (funcall method indent-point state))))))))

    ;; (add-hook 'emacs-lisp-mode-hook
    ;;           (lambda () (setq-local lisp-indent-function #'my-lisp-indent-function)))
    ))

;; Interactive macro expander
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
         ("C-c e" . macrostep-expand)
         :map lisp-interaction-mode-map
         ("C-c e" . macrostep-expand)))

(use-package help-mode
  :straight (:type built-in)
  :bind
  ("C-h K" . describe-keymap)
  (:map help-mode-map
   ("<" . help-go-back)
   (">" . help-go-forward)))
;; -*- lexical-binding: t; -*-

(use-package lispy
  :straight (:type git :host github :repo "abo-abo/lispy"))

(use-package symex
  :straight (:type git :host github :repo "drym-org/symex.el")
  :config
  (symex-initialize)
  (global-set-key (kbd "s-;") 'symex-mode-interface))

(use-package helpful
  :preface
  :bind
  ("C-h c" . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-function] . helpful-function)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-symbol] . helpful-symbol)
  :custom
  (helpful-switch-buffer-function #'display-buffer)
  :config
  (add-to-list 'display-buffer-alist
               '("*helpful.*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.33))))

(use-package help-fns
  :straight (:type built-in)
  :bind
  ("C-h F" . describe-face))

(use-package paren-face
  :hook (emacs-lisp-mode . paren-face-mode))

(use-package elisp-demos
  :straight t
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  (advice-add 'describe-function-1
              :after #'elisp-demos-advice-describe-function-1))


(use-package elisp-def
  :hook (emacs-lisp-mode . elisp-def-mode))

(use-package eros :hook (emacs-lisp-mode . eros-mode))

(use-package mode-minder
  :straight (:type git :host github :repo "jdtsmith/mode-minder"))

(use-package geiser)

(use-package geiser-racket :after geiser)

(use-package racket-mode
  :bind (:map racket-repl-mode-map
         ("C-RET" . newline-and-indent)))

(provide 'init-elisp)
;;; init-elisp.el ends here
