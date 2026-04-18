;; -*- lexical-binding: t; -*-

(use-package tramp
  ;; C-x C-f /xxx@yyyy:~/.abcd
  :straight (:type built-in)
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 1)


  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))

  ;; no lockfiles on remote
  (setq create-lockfiles nil)

  ;; Keep auto-saves local in /tmp
  (setq auto-save-file-name-transforms
        '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
           "/tmp/tramp-autosave/\\2" t)))

  ;; skip vc on remote files
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  ;; don't expire cache aggressively
  (setq remote-file-name-inhibit-cache nil))

(provide 'init-tramp)
