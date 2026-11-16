;;; diff-review-herdr-test.el --- Review handoff tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'diff-review-herdr)
(require 'herdr)
(require 'server)
(require 'evil)

(defvar diff-review-herdr-test--root)
(defvar diff-review-herdr-test--worktree)
(defvar diff-review-herdr-test--review)
(defvar diff-review-herdr-test--submission)
(defvar diff-review-herdr-test--snapshot)

(defun diff-review-herdr-test--git (directory &rest args)
  (apply #'diff-review--git-at directory args))

(defun diff-review-herdr-test--comment (id &optional resolved)
  (let ((comment (diff-review-herdr--object
                  "id" id "path" "a.txt" "lines" [2 2]
                  "blob" (diff-review-herdr-test--git diff-review-herdr-test--worktree
                                                     "rev-parse" "HEAD:a.txt")
                  "author" "Reviewer" "at" "2026-11-16T00:00:00Z"
                  "body" (concat "Feedback " id) "replies" [] "severity" "keep-me")))
    (when resolved (puthash "resolved" t comment))
    comment))

(defmacro diff-review-herdr-test--fixture (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "diff-review-herdr-test-" t))
          (diff-review-herdr-test--root (expand-file-name "main/" directory))
          (diff-review-herdr-test--worktree (expand-file-name "feature worktree/" directory))
          (diff-review-herdr-test--review
           (expand-file-name ".git/review/feature.json" diff-review-herdr-test--root))
          (diff-review-sidebar nil)
          (diff-review-herdr-test--submission nil)
          (diff-review-herdr-test--snapshot nil)
          (default-directory directory)
          ;; Git integrations/hooks can spawn background note writers.  Keep
          ;; scratch repositories independent of the user's Git configuration.
          (exec-path (cons "/usr/bin" exec-path))
          (process-environment (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                                       process-environment)))
     (unwind-protect
         (save-window-excursion
           (make-directory diff-review-herdr-test--root)
           (diff-review-herdr-test--git diff-review-herdr-test--root "init" "-b" "main")
           (diff-review-herdr-test--git diff-review-herdr-test--root "config" "user.name" "Test")
           (diff-review-herdr-test--git diff-review-herdr-test--root "config" "user.email" "test@example.invalid")
           (with-temp-file (expand-file-name "a.txt" diff-review-herdr-test--root)
             (insert "first\nlast\n"))
           (diff-review-herdr-test--git diff-review-herdr-test--root "add" "a.txt")
           (diff-review-herdr-test--git diff-review-herdr-test--root "commit" "-m" "Base")
           (diff-review-herdr-test--git diff-review-herdr-test--root "worktree" "add" "-b" "feature"
                                      diff-review-herdr-test--worktree)
           (with-temp-file (expand-file-name "a.txt" diff-review-herdr-test--worktree)
             (insert "first\nnew\nlast\n"))
           (diff-review-herdr-test--git diff-review-herdr-test--worktree "commit" "-am" "Feature")
           (diff-review-herdr--write
            diff-review-herdr-test--review
            (diff-review-herdr--object
             "format" "diff-review/1" "root" diff-review-herdr-test--root
             "base" "main" "head" "feature" "viewed" (make-hash-table :test #'equal)
             "custom_data" "preserved"
             "comments" (vector (diff-review-herdr-test--comment "one")
                                (diff-review-herdr-test--comment "done" t))))
           ;; Exercise emacs-herdr's real snapshot extraction and picker formatting.
           (setq diff-review-herdr-test--snapshot
                 `((result . ((snapshot . ((agents .
                                            [((agent_status . "idle") (agent . "codex")
                                              (name . "worker") (pane_id . "w1:p1")
                                              (cwd . ,diff-review-herdr-test--root)
                                              (terminal_title_stripped . "Outcome events"))])
                                          (workspaces . []) (tabs . [])))))))
           (cl-letf (((symbol-function 'herdr--call-json-sync)
                      (lambda (&rest _args) diff-review-herdr-test--snapshot))
                     ((symbol-function 'herdr--call-async)
                      (lambda (callback &rest args)
                        (setq diff-review-herdr-test--submission args)
                        (funcall callback '((ok . t)))))
                     ((symbol-function 'completing-read)
                      (lambda (_prompt collection &rest _args) (caar collection))))
             (diff-review-find-file diff-review-herdr-test--review
                                    diff-review-herdr-test--root "main" "feature")
             ,@body))
       (dolist (buffer (buffer-list))
         (when (and (buffer-file-name buffer)
                    (file-in-directory-p (buffer-file-name buffer) directory))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory directory t))))

(defmacro diff-review-herdr-test--fake-client (&rest body)
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'diff-review-herdr--client) (lambda () ["/bin/false"])))
     ,@body))

(defun diff-review-herdr-test--batch-file ()
  (gethash "batch" (diff-review-herdr--metadata)))

(defun diff-review-herdr-test--result (id action body)
  (diff-review-herdr-apply-result
   (base64-encode-string
    (encode-coding-string
     (json-serialize
      (diff-review-herdr--object "batch" (diff-review-herdr-test--batch-file)
                                "thread" id "action" action "body" body)) 'utf-8) t)))

(ert-deftest diff-review-herdr-targets-feature-worktree-from-main ()
  (diff-review-herdr-test--fixture
    (let ((target (diff-review-herdr--target)))
      (should (file-equal-p (gethash "worktree" target) diff-review-herdr-test--worktree))
      (should (equal (gethash "branch" target) "feature")))
    (puthash "head" (diff-review-herdr-test--git diff-review-herdr-test--worktree
                                                "rev-parse" "HEAD") diff-review--data)
    (should-error (diff-review-herdr--target) :type 'user-error)))

(ert-deftest diff-review-herdr-keys-work-in-evil-normal-state ()
  (diff-review-herdr-test--fixture
    (evil-local-mode 1)
    (evil-normal-state)
    (should (eq (key-binding (kbd "S")) #'diff-review-herdr-send))
    (should (eq (key-binding (kbd "B")) #'diff-review-herdr-bind-agent))
    (should (eq (key-binding (kbd "I")) #'diff-review-herdr-status))
    (should (eq (key-binding (kbd "C")) #'diff-review-request-changes))))

(ert-deftest diff-review-herdr-worktree-review-has-no-commit-verdict ()
  (diff-review-herdr-test--fixture
    (diff-review--modify-comments (lambda () (remhash "head" diff-review--data)))
    (diff-review-refresh)
    (diff-review-herdr-test--fake-client (diff-review-herdr-send))
    (should (file-equal-p (gethash "worktree"
                                 (diff-review-herdr--read (diff-review-herdr-test--batch-file)))
                         diff-review-herdr-test--root))
    (should-not (gethash "verdict" diff-review--data))))

(ert-deftest diff-review-herdr-sends-open-threads-with-explicit-paths ()
  (diff-review-herdr-test--fixture
    (diff-review-herdr-test--fake-client (diff-review-herdr-send))
    (let* ((batch (diff-review-herdr--read (diff-review-herdr-test--batch-file)))
           (prompt (nth 3 diff-review-herdr-test--submission)))
      (should (equal (seq-take diff-review-herdr-test--submission 3)
                     '("agent" "prompt" "w1:p1")))
      (should (= 1 (length (gethash "threads" batch))))
      (should (equal (gethash "delivery" batch) "accepted"))
      (should (string-search diff-review-herdr-test--worktree prompt))
      (should (string-search diff-review-herdr-test--review prompt))
      (should (string-search "resolve THREAD_ID" prompt))
      (should (equal (gethash "state" (gethash "verdict" diff-review--data)) "changes"))
      (should-not (gethash "resolved" (diff-review--comment "one")))
      (should-error (diff-review-herdr-send) :type 'user-error))))

(ert-deftest diff-review-herdr-reuses-binding-and-rejects-replaced-agent ()
  (diff-review-herdr-test--fixture
    (diff-review-herdr-bind-agent)
    (diff-review-refresh)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "Asked twice"))))
      (should (equal (alist-get 'pane-id
                               (diff-review-herdr--bound-agent (diff-review-herdr--target) nil))
                     "w1:p1")))
    (setf (alist-get 'agent
                    (aref (alist-get 'agents (alist-get 'snapshot
                                                       (alist-get 'result diff-review-herdr-test--snapshot))) 0))
          "claude")
    (should-error (diff-review-herdr--bound-agent (diff-review-herdr--target) nil)
                  :type 'user-error)))

(ert-deftest diff-review-herdr-does-not-submit-to-busy-agents-or-empty-reviews ()
  (diff-review-herdr-test--fixture
    (setf (alist-get 'agent_status
                    (aref (alist-get 'agents (alist-get 'snapshot
                                                       (alist-get 'result diff-review-herdr-test--snapshot))) 0))
          "working")
    (should-error (diff-review-herdr-send) :type 'user-error)
    (should-not diff-review-herdr-test--submission)
    (diff-review--modify-comments (lambda () (puthash "resolved" t (diff-review--comment "one"))))
    (should-error (diff-review-herdr-send) :type 'user-error)
    (should-not diff-review-herdr-test--submission)))

(ert-deftest diff-review-herdr-result-preserves-new-feedback-and-unknown-data ()
  (diff-review-herdr-test--fixture
    (diff-review-herdr-test--fake-client (diff-review-herdr-send))
    ;; A separate writer adds feedback while this Emacs still holds the old data.
    (let ((disk (diff-review-herdr--read diff-review-herdr-test--review)))
      (puthash "comments" (vconcat (gethash "comments" disk)
                                   (vector (diff-review-herdr-test--comment "new"))) disk)
      (diff-review-herdr--write diff-review-herdr-test--review disk))
    (should (equal (diff-review-herdr-test--result "one" "resolve" "Fixed in abc123") "OK"))
    (should (gethash "resolved" (diff-review--comment "one")))
    (should (= 1 (length (gethash "replies" (diff-review--comment "one")))))
    (should (equal (gethash "severity" (diff-review--comment "one")) "keep-me"))
    (should (equal (gethash "custom_data" diff-review--data) "preserved"))
    (should-not (gethash "resolved" (diff-review--comment "new")))
    (should (equal (gethash "state" (diff-review-herdr--read
                                     (diff-review-herdr-test--batch-file))) "complete"))))

(ert-deftest diff-review-herdr-reply-leaves-thread-open-and-allows-another-round ()
  (diff-review-herdr-test--fixture
    (diff-review-herdr-test--fake-client
      (diff-review-herdr-send)
      (diff-review-herdr-test--result "one" "reply" "Need clarification")
      (should-not (gethash "resolved" (diff-review--comment "one")))
      (diff-review-herdr-send)
      (let* ((batch (diff-review-herdr--read (diff-review-herdr-test--batch-file)))
             (comment (gethash "comment" (aref (gethash "threads" batch) 0))))
        (should (= 1 (length (gethash "replies" comment))))))))

(ert-deftest diff-review-herdr-batch-finishes-only-after-every-thread-reports ()
  (diff-review-herdr-test--fixture
    (diff-review--modify-comments
     (lambda () (setq diff-review--comments
                     (vconcat diff-review--comments
                              (vector (diff-review-herdr-test--comment "two"))))))
    (diff-review-herdr-test--fake-client (diff-review-herdr-send))
    (diff-review-herdr-test--result "one" "resolve" "Fixed")
    (should (diff-review-herdr--active-batch))
    (should-error (diff-review-herdr-send) :type 'user-error)
    (diff-review-herdr-test--result "two" "reply" "Need clarification")
    (should-not (diff-review-herdr--active-batch))
    (should (gethash "resolved" (diff-review--comment "one")))
    (should-not (gethash "resolved" (diff-review--comment "two")))))

(ert-deftest diff-review-herdr-result-does-not-overwrite-unsaved-review ()
  (diff-review-herdr-test--fixture
    (diff-review-herdr-test--fake-client (diff-review-herdr-send))
    (set-buffer-modified-p t)
    (should-error (diff-review-herdr-test--result "one" "resolve" "Fixed") :type 'user-error)
    (should (buffer-modified-p))
    (should-not (gethash "resolved" (diff-review--comment "one")))))

(ert-deftest diff-review-herdr-client-does-not-connect-to-another-emacs ()
  (let ((server-name "another-emacs") (server-process nil))
    (cl-letf (((symbol-function 'server-running-p) (lambda (&rest _) t))
              ((symbol-function 'server-start) (lambda (&rest _) (ert-fail "Started a server"))))
      (should-error (diff-review-herdr--client) :type 'user-error))))

(ert-deftest diff-review-herdr-rejects-edited-threads-and-unknown-ids ()
  (diff-review-herdr-test--fixture
    (diff-review-herdr-test--fake-client (diff-review-herdr-send))
    (diff-review--modify-comments
     (lambda () (puthash "body" "Updated request" (diff-review--comment "one"))))
    (should-error (diff-review-herdr-test--result "one" "resolve" "Old fix") :type 'user-error)
    (should-error (diff-review-herdr-test--result "not-submitted" "resolve" "Oops") :type 'user-error)
    (should-not (gethash "resolved" (diff-review--comment "one")))
    (should (seq-empty-p (gethash "replies" (diff-review--comment "one"))))))

(ert-deftest diff-review-herdr-retries-do-not-duplicate-or-reresolve-reopened-threads ()
  (diff-review-herdr-test--fixture
    (diff-review-herdr-test--fake-client (diff-review-herdr-send))
    (diff-review-herdr-test--result "one" "resolve" "Fixed")
    (diff-review--modify-comments (lambda () (remhash "resolved" (diff-review--comment "one"))))
    (diff-review-herdr-test--result "one" "resolve" "Fixed")
    (should-not (gethash "resolved" (diff-review--comment "one")))
    (should (= 1 (length (gethash "replies" (diff-review--comment "one")))))
    (should-error (diff-review-herdr-test--result "one" "reply" "Different result") :type 'user-error)))

(ert-deftest diff-review-herdr-rejects-results-after-cancel-or-branch-switch ()
  (diff-review-herdr-test--fixture
    (diff-review-herdr-test--fake-client (diff-review-herdr-send))
    (diff-review-herdr-test--git diff-review-herdr-test--worktree "checkout" "-b" "other")
    (should-error (diff-review-herdr-test--result "one" "resolve" "Wrong branch") :type 'user-error)
    (diff-review-herdr-test--git diff-review-herdr-test--worktree "checkout" "feature")
    (diff-review-herdr-cancel)
    (should-error (diff-review-herdr-test--result "one" "resolve" "Late result") :type 'user-error)
    (should-not (gethash "resolved" (diff-review--comment "one")))))

(ert-deftest diff-review-herdr-unconfirmed-delivery-is-not-retried ()
  (diff-review-herdr-test--fixture
    (diff-review-herdr-test--fake-client (diff-review-herdr-send))
    (diff-review-herdr--delivery-finished (diff-review-herdr-test--batch-file) nil)
    (should (equal (gethash "delivery" (cdr (diff-review-herdr--active-batch))) "unconfirmed"))
    (should-error (diff-review-herdr-send) :type 'user-error)
    (should-not (gethash "resolved" (diff-review--comment "one")))))

(defun diff-review-herdr-test--run-helper (file action id &rest args)
  "Run the real Python helper, servicing this Emacs's local server."
  (let* ((buffer (generate-new-buffer " *review-helper-test*"))
         (process (make-process :name "review-helper-test" :buffer buffer
                                :command (append (list diff-review-herdr--helper "--batch" file action id) args)
                                :noquery t :connection-type 'pipe))
         (deadline (+ (float-time) 10)))
    (unwind-protect
        (progn
          (while (and (process-live-p process) (< (float-time) deadline))
            (accept-process-output nil 0.05))
          (when (process-live-p process) (ert-fail "Helper did not finish"))
          (cons (process-exit-status process) (with-current-buffer buffer (buffer-string))))
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(ert-deftest diff-review-herdr-helper-round-trip-over-real-emacs-socket ()
  (diff-review-herdr-test--fixture
    (let* ((server-name "review-test")
           (server-socket-dir (make-temp-file "/tmp/drh-socket-" t))
           (server-use-tcp nil)
           (server-process nil)
           (body "Removed it. Quotes: \" ' \\; Unicode: 日本語.\nSecond line: $(false).")
           (body-file (expand-file-name "reply.txt" default-directory)))
      (unwind-protect
          (progn
            (diff-review--modify-comments
             (lambda () (puthash "body" "日本語のコメント" (diff-review--comment "one"))))
            (diff-review-herdr-send)
            (with-temp-file body-file (insert body))
            (let ((result (diff-review-herdr-test--run-helper
                           (diff-review-herdr-test--batch-file) "resolve" "one"
                           "--body-file" body-file)))
              (should (zerop (car result))))
            (should (gethash "resolved" (diff-review--comment "one")))
            (should (equal body (gethash "body" (aref (gethash "replies" (diff-review--comment "one")) 0))))
            (diff-review--modify-comments
             (lambda () (puthash "unrelated" "preserved" diff-review--data)))
            (diff-review-refresh)
            (should (equal body (gethash "body" (aref (gethash "replies" (diff-review--comment "one")) 0))))
            (should (equal "日本語のコメント" (gethash "body" (diff-review--comment "one")))))
        (when (process-live-p server-process) (delete-process server-process))
        (delete-directory server-socket-dir t)))))

(ert-deftest diff-review-herdr-helper-reports-disconnected-emacs ()
  (diff-review-herdr-test--fixture
    (diff-review-herdr-test--fake-client (diff-review-herdr-send))
    (let ((result (diff-review-herdr-test--run-helper
                   (diff-review-herdr-test--batch-file) "resolve" "one" "--body" "Fixed")))
      (should (= 1 (car result)))
      (should (string-search "unconfirmed" (cdr result))))
    (should-not (gethash "resolved" (diff-review--comment "one")))))

;;; diff-review-herdr-test.el ends here
