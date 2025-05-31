;; -*- lexical-binding: t; -*-
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (defun khz/nov-font-setup()
    (face-remap-add-relative 'variable-pitch :family "Vollkorn" :height 1.2)))

(use-package nov-xwidget
  :straight (:type git :host github :repo "chenyanming/nov-xwidget")
  :after nov
  :hook (nov-mode . nov-xwidget-inject-all-files)
  :config
  (define-key nov-mode-map (kbd "o") 'nov-xwidget-view))

(use-package emojify)

(use-package calibredb)

(use-package bookmark-plus
  :straight (bookmark-plus :type git :host github :repo "emacsmirror/bookmark-plus")
  :defer 3
  :init
  (require 'bookmark+)
  ;; save bookmark on every change
  (setq bookmark-save-flag 1))

(use-package bookmark-view
  :straight (bookmark-view :type git :host github :repo "minad/bookmark-view"))

(use-package bookmark-view
  :ensure nil
  :commands my-bookmark-view-create
  :bind*
  (("M-8" . my-bookmark-view-previous)
   ("M-9" . my-bookmark-view-next)
   ("M-0" . my-bookmark-view-dwim)
   ("M-1" . my-bookmark-view)
   ("M-2" . my-bookmark-view)
   ("M-3" . my-bookmark-view)
   ("M-4" . my-bookmark-view)
   ("M-5" . my-bookmark-view)
   ("M-6" . my-bookmark-view))
  :config
  (setq bookmark-view-name-format "<count> <buffers>"
        bookmark-view-name-regexp "\\`[0-9]+ ")

  (defun bookmark-view-buffer-names ()
    (string-join (sort (mapcar #'buffer-name (bookmark-view--buffers nil))
                       #'string-lessp) " "))

  (defun my-bookmark-view-dwim (&optional arg)
    (interactive "P")
    (if arg (bookmark-view-delete)
      (if (my-bookmark-view-after-hopping-p)
          (my-bookmark-view "0")
        (call-interactively 'bookmark-view))))

  (defun my-bookmark-rotate (forward)
    ;; Two functions append to a list: append and nconc, both would want a list as the append value
    ;; which I create with (cons 'value ())
    (if forward
        (let ((target (pop bookmark-view-history)))
          (add-to-list 'bookmark-view-history target t)
          target)
      (let ((target (last bookmark-view-history)))
        (setq bookmark-view-history
              (nconc target (butlast bookmark-view-history)))
        (car target))))

  (defun my-bookmark-view-after-hopping-p ()
    (or (equal last-command 'my-bookmark-view-previous)
        (equal last-command 'my-bookmark-view-next)))

  (defun my-bookmark-view-previous ()
    (interactive)
    (unless (my-bookmark-view-after-hopping-p)
      ;; save place-before-hopping to slot 0
      (my-bookmark-view-create "0" 'omit-history))
    (when bookmark-view-history
      (when (equal last-command 'my-bookmark-view-next)
        (my-bookmarkrotate t))
      (bookmark-view-open (my-bookmark-rotate t))))

  (defun my-bookmark-view-next ()
    (interactive)
    (unless (my-bookmark-view-after-hopping-p)
      ;; save place-before-hopping to slot 0
      (my-bookmark-view-create "0" 'omit-history))
    (when bookmark-view-history
      (when (equal last-command 'my-bookmark-view-previous)
        (my-bookmark-rotate nil))
      (bookmark-view-open (my-bookmark-rotate nil))))

  (defun bookmark-view--get-slot (slot &optional delete)
    (let ((slot-name (seq-find (apply-partially #'string-match-p (format "\\`%s:" slot))
                               (bookmark-view-names))))
      (and slot-name delete
           (progn (bookmark-view-delete slot-name)
                  (setq bookmark-view-history
                        (delete slot-name bookmark-view-history))))
      slot-name))

  (defun my-bookmark-view (&optional slot)
    (interactive)
    (let ((slot (or slot (substring (key-description (this-command-keys-vector)) -1))))
      (let ((slot-name (bookmark-view--get-slot slot)))
        (if slot-name (bookmark-view slot-name)
          (message "No view in slot %s" slot)))))

  (defun my-bookmark-view-create (&optional slot omit-history)
    (interactive)
    (let* ((slot (or slot (substring (key-description (this-command-keys-vector)) -1)))
           (name (format "%s:%s" slot (bookmark-view-buffer-names))))
      (bookmark-view--get-slot slot 'delete)
      (bookmark-view-save name nil)
      (unless omit-history
        (add-to-history 'bookmark-view-history name))
      ;; (add-to-ordered-list 'bookmark-view-history name 0)
      (message "View saved to slot %s" slot))))

;; (use-package mugur
;;  :straight (mugur :type git :host github :repo "mihaiolteanu/mugur"))

(use-package devdocs
  :straight (:type git :host github :repo "astoff/devdocs.el")
  :bind ("C-h D" . devdocs-lookup))

;; generate uuid
(global-set-key (kbd "C-c C-'") 'uuidgen)

(use-package writegood-mode
  :straight (:type git :host github :repo "bnbeckwith/writegood-mode"))

(use-package artbollocks-mode
  :straight (:type git :host github :repo "sachac/artbollocks-mode"))

(use-package ultra-scroll
  :straight (:type git :host github :repo "jdtsmith/ultra-scroll")
  :init (setq scroll-conservatively 101
              scroll-margin 0)
  :config (ultra-scroll-mode 1))

(use-package leetcode
  :straight (:type git :host github :repo "kaiwk/leetcode.el")
  :config (setq leetcode-prefer-language "python3"
                leetcode-prefer-sql "mysql"
                leetcode-save-solutions t
                leetcode-directory "~/Code/algorithms/leetcode"))

(define-advice zone (:around (orig-fn &rest _) "zone-all-buffer")
  (save-window-excursion
    (let ((op-win (car (window-list))))
      (mapc (lambda (w)
              (with-selected-window w
                (switch-to-buffer "*zone*")))
            (cdr (window-list)))
      (with-selected-window op-win
        (funcall orig-fn)))))

(use-package leetcode-emacs
  :disabled t
  :straight (:host github :repo "ginqi7/leetcode-emacs")
  :config
  (setq leetcode-language "python3"))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

(use-package monkeytype
  :straight (:type git :host github :repo "jpablobr/emacs-monkeytype"))

(use-package fretboard
  :straight (:host github :repo "skyefreeman/fretboard.el"))

(provide 'init-others)
