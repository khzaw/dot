
(straight-use-package 'circe)
(straight-use-package 'websocket)
(straight-use-package 'alert)
(use-package slack
  :straight (:type git :host github :repo "yuya373/emacs-slack")
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "deliveryhero"
   :default t
   :token "xoxc-5091155421-1311640293766-7285171866613-f42c00accb1892b2ebfcf74b1ceb3ea4ccc4992279bb0164d2124ffe2333c42e"
   :cookie "xoxd-4flbIkwt6%2BETui%2BPgNo51BdayDPBXCLrw7hL2jGV0BJhcVXzMT%2F9hc8pq%2FeHMsMv5cMgdz2MfDCt5NLa%2F3Uy2XfK7punITdG%2BfJ3zQcsPo57lL%2BIB0HWtjJi0lj6Oc%2BHQ%2BiHkmTddqtJeyjhtp4Mm9UODHxJOPmOJi4u9QxmsFE%2B40vBBqPX2l9meLlpf28TGk%2FrJ0J0xZI%3D"
   :subscribed-channels '(pd-tx-pablo-posse)))

(provide 'init-slack)
