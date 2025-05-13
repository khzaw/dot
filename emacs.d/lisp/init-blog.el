;; -*- lexical-binding: t; -*-

(use-package easy-hugo
  :straight (:type git :host github :repo "masasam/emacs-easy-hugo")
  :init
  (setq easy-hugo-basedir "~/Code/blog/"
        easy-hugo-postdir "content/posts"
        easy-hugo-default-ext ".org")
  :config (easy-hugo-enable-menu))
