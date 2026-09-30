;; -*- lexical-binding: t; -*-

(use-package tramp
  ;; C-x C-f /xxx@yyyy:~/.abcd
  :straight (:type built-in)
  :defer t
  :init
  ;; no lockfiles
  (setq create-lockfiles nil)

  ;; Keep auto-saves local in /tmp
  (setq auto-save-file-name-transforms
        '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
           "/tmp/tramp-autosave/\\2" t)))

  (setq remote-file-name-inhibit-cache nil)
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 1)


  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))

  ;; skip vc on remote files
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(provide 'init-tramp)
