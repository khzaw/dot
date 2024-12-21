;; -*- lexical-binding: t; -*-

(use-package org-jira
  :straight (:type git :host github :repo "ahungry/org-jira")
  :custom
  (jiralib-url "https://deliveryhero.atlassian.net")
  :preface
  (defun khz-org-jira-get-issues-assigned-to-me ()
    "Get all issues assigned to me."
    (interactive)
    (org-jira-get-issues-from-custom-jql '((:jql "assignee = currentUser() AND resolution = Unresolved ORDER BY priority DESC, updated DESC"
                                            :limit 1000
                                            :filename "jira-assigned-to-me"))))
  :config
  (setq org-jira-jira-status-to-org-keyword-alist '(("To Do" . "TODO")
                                                    ("Ready for Development" . "TODO")
                                                    ("In Progress" . "DOING")
                                                    ("Done" . "DONE")
                                                    ("Closed" . "DONE")
                                                    ("Canceled" . "CANCELLED")
                                                    ("Blocked" . "BLOCKED")
                                                    ("Ready for Review" . "REVIEW")
                                                    ("Code Review" . "REVIEW"))))

(provide 'init-jira)
