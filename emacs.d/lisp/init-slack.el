(straight-use-package 'circe)
(straight-use-package 'websocket)
(straight-use-package 'alert)
(straight-use-package 'ts)
(straight-use-package 'oauth2)
(straight-use-package 'request)
(use-package emacs-slack
  :straight (:type git :host github :repo "emacs-slack/emacs-slack")
  :init
  (setq slack-quick-update t)
  (setq slack-prefer-current-team t)
  (setq slack-buffer-emojify t)
  :bind (("C-c S K" . slack-stop)
         ("C-c S c" . slack-select-rooms)
         ("C-c S u" . slack-select-unread-rooms)
         ("C-c S U" . slack-user-select)
         ("C-c S s" . slack-search-from-messages)
         ("C-c S J" . slack-jump-to-browser)
         ("C-c S j" . slack-jump-to-app)
         ("C-c S e" . slack-insert-emoji)
         ("C-c S E" . slack-message-edit)
         ("C-c S r" . slack-message-add-reaction)
         ("C-c S t" . slack-thread-show-or-create)
         ("C-c S g" . slack-message-redisplay)
         ("C-c S G" . slack-conversations-list-update-quick)
         ("C-c S q" . slack-quote-and-reply)
         ("C-c S Q" . slack-quote-and-reply-with-link)
         (:map slack-mode-map
          (("@" . slack-message-embed-mention)
           ("#" . slack-message-embed-channel)))
         (:map slack-thread-message-buffer-mode-map
          (("C-c '" . slack-message-write-another-buffer)
           ("@" . slack-message-embed-mention)
           ("#" . slack-message-embed-channel)))
         (:map slack-message-buffer-mode-map
          (("C-c '" . slack-message-write-another-buffer)))
         (:map slack-message-compose-buffer-mode-map
          (("C-c '" . slack-message-send-from-buffer))))
  ;; :custom
  (slack-extra-subscribed-channels (mapcar 'intern (list "pd-tx-pablo-posse"
                                                         "pd-prodtech-ms-otp_revamp-tech")))
  :config
  (slack-register-team
   :name "deliveryhero"
   :token "xoxc-2315236468359-1311640293766-8064054650595-039367d0d1875e9b170e93ce2d073c7846c26ae760b978234b121e0132fffe11"
   :cookie "xoxd-Ts2LR7JdookBpyDTy552yvaI8WG4zJMi6mEAZK%2B66jvtIaVzJQfUsiSZ4fcUt5lS7BDixP0XErad8V5AK2%2BHv8O8TOUreTfbLxR132WzWzzLEGo7RPgZw%2FrFWyblu0MDVvTO1VHsmGbucBxpfQSYUq6wH8NU2Txjhcvr8976uo16jo9wTIVhrX2o4hvsV0sxaT2GZu%2F7JBv9RT4G9reDEmdrK8lb; d-s=1730898019; lc=1732194742"
   :default t
   :subscribed-channels nil))

(provide 'init-slack)
