;; -*- lexical-binding: t; -*-

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind (("C-c p" . khz/load-projectile-prefix)
         :map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :preface
  (defun khz/ensure-projectile ()
    "Load Projectile, allowing integrations to install project prefix keys."
    (let* ((prefix (kbd "C-c p"))
           (loader (eq (lookup-key global-map prefix)
                       #'khz/load-projectile-prefix)))
      (when loader
        (global-unset-key prefix))
      (condition-case err
          (require 'projectile)
        (error
         (when loader
           (global-set-key prefix #'khz/load-projectile-prefix))
         (signal (car err) (cdr err))))))

  (defun khz/load-projectile-prefix ()
    "Load Projectile and replay the project prefix key sequence."
    (interactive)
    (khz/ensure-projectile)
    (setq unread-command-events
          (append (mapcar (lambda (event) (cons t event))
                          (listify-key-sequence (this-command-keys-vector)))
                  unread-command-events)))

  (defun khz/project-try-projectile (directory)
    "Load Projectile when project detection first needs DIRECTORY."
    (khz/ensure-projectile)
    (project-projectile directory))
  :init
  (khz/run-on-idle #'khz/ensure-projectile 0.8)
  (with-eval-after-load 'project
    (unless (featurep 'projectile)
      (add-hook 'project-find-functions #'khz/project-try-projectile)))
  :custom
  (projectile-sort-order 'recentf)
  (projectile-use-git-grep t)
  (projectile-enable-caching t)
  (projectile-verbose nil)
  (projectile-completion-system 'default)
  :config
  (remove-hook 'project-find-functions #'khz/project-try-projectile)
  (projectile-mode 1)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  :bind (:map projectile-mode-map
         ("C-c p B" . consult-projectile)
         ("C-c p f" . consult-projectile-find-file)))


;; (use-package project
;;   :pin gnu
;;   :bind (("C-c k" . #'project-kill-buffers)
;;           ("C-c m" . #'project-compile)
;;           ("C-x f" . #'find-file)
;;           ("C-c f" . #'project-find-file)
;;           ("C-c F" . #'project-switch-project))
;;   :custom
;;   (project-switch-commands
;;     '((project-find-file "Find file")
;;        (magit-project-status "Magit" ?g)
;;        (deadgrep "Grep" ?h)))
;;   (compilation-always-kill t)
;;   (project-vc-merge-submodules nil)
;;   )

(provide 'init-projectile)
;;; init-projectile.el ends here
