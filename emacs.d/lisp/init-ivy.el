(use-package ag)

(use-package fzf)

(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("C-s" . swiper-isearch)
          ("C-r" . swiper-isearch-backward)
          ("C-c C-r" . ivy-resume)

          ("M-x" . counsel-M-x)
          ;; ("C-x b" . counsel-switch-buffer)
          ("C-x C-f" . counsel-find-file)
          ("C-c i" . counsel-imenu)
          ("C-c l t" . counsel-load-theme)
          ("C-h f" . counsel-describe-function)
          ("C-h v" . counsel-describe-variable)
          ("C-h o" . counsel-describe-symbol)
          :map counsel-mode-map
          ([remap swiper] . counsel-grep-or-swiper)
          ([remap swiper-backward] . counsel-grep-or-swiper-backward)
          ([remap dired] . counsel-dired)
          ([remap set-variable] . counsel-set-variable)
          ([remap insert-char] . counsel-unicode-char)
          ([remap recentf-open-files] . consel-recentf)
          ([remap org-capture] . counsel-org-capture)

          ("C-h F" . counsel-faces)
          ("C-c P" . counsel-package)
          ("C-c a" . counsel-ag)
          ("C-c f" . counsel-projectile-find-file)
          ("C-c g f" . fzf-git-files)

          :map ivy-minibuffer-map
          ("C-w" . ivy-yank-word)

          :map counsel-find-file-map
          ("C-h" . counsel-up-directory))
  :hook ((after-init . ivy-mode)
          (ivy-mode . counsel-mode))
  :init
  (setq enable-recursive-minibuffers t) ; allow commands in minibuffers
  (setq ivy-dynamic-exhibit-delay-ms 250)

  (setq ivy-height 12
    ivy-use-selectable-prompt t
    ivy-use-virtual-buffers t ; enable bookmarks and recentf
    ivy-fixed-height-minibuffer t
    ivy-count-format "(%d/%d) "
    ivy-ignore-buffers '("\\` " "\\`\\*tramp/" "\\`\\*xref" "\\`\\*helpful "
                          "\\`\\*.+-posframe-buffer\\*" "\\` ?\\*company-.+\\*")
    ivy-on-del-error-function #'ignore
    ivy-initial-inputs-alist nil
    ivy-display-style 'fancy)
  ;; Use orderless regex strategy
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

  ;; Set minibuffer height for different commands
  (setq ivy-height-alist '((counsel-evil-registers . 5)
                            (counsel-yank-pop . 8)
                            (counsel-git-log . 4)
                            (swiper . 15)
                            (counsel-projectile-ag . 15)
                            (counsel-projectile-rg . 15)))

  (setq swiper-action-recenter t)

  (setq counsel-find-file-at-point t
    counsel-preselect-current-file t)
  ;; Be compatible with `gls'
  (when (and (eq system-type 'darwin) (executable-find "gls"))
    (setq counsel-find-file-occur-use-find nil
      counsel-find-file-occur-cmd
      "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first"))


  :config
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :hook ((counsel-projectile-mode . ivy-rich-mode) ; MUST after `counsel-projectile'
         (ivy-rich-mode . ivy-rich-project-root-cache-mode)
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))

(use-package amx)

(use-package ivy-posframe
  :after (ivy posframe)
  :config (setq ivy-posframe-parameters ''((internal-border-width . 10))))

(use-package ivy-avy
  :bind (:map ivy-minibuffer-map
          ("C-'" . ivy-avy)))

(use-package all-the-icons-ivy-rich :after ivy :config (all-the-icons-ivy-rich-mode))

(use-package counsel-projectile
  :hook (counsel-mode . counsel-projectile-mode)
  :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

(use-package counsel-spotify
  :after counsel
  :init
  (setq counsel-spotify-client-id 'spotify-client-id
        counsel-spotify-client-secret 'spotify-secret)
  :bind (("C-c C-s p" . counsel-spotify-toggle-play-pause)
         ("C-c C-s n" . counsel-spotify-next)
         ("C-c C-s r" . counsel-spotify-prev)
         ("C-c C-s s" . counsel-spotify-search-track)
         ("C-c C-s a" . counsel-spotify-search-artist)
         ("C-c C-s l" . counsel-spotify-search-playlist)))


(provide 'init-ivy)
;;; init-ivy.el ends here
