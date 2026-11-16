;;; diff-review-herdr.el --- Send local reviews to Herdr -*- lexical-binding: t; -*-

;;; Commentary:
;; A review owns its agent binding and one feedback batch at a time.  The
;; helper in ../bin reports individual results through emacsclient; Emacs is
;; the only writer of review threads.  No terminal output is interpreted.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'diff-review)

(declare-function herdr--call-json-sync "herdr" (&rest args))
(declare-function herdr--call-async "herdr" (callback &rest args))
(declare-function herdr--extract-agents "herdr" (json))
(declare-function herdr--format-candidate "herdr" (agent))
(declare-function herdr--sort-agents "herdr" (agents))
(declare-function server-running-p "server" (&optional name))
(declare-function server-start "server" (&optional leave-dead))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(defvar server-name)
(defvar server-socket-dir)
(defvar server-auth-dir)
(defvar server-use-tcp)
(defvar server-process)

(defconst diff-review-herdr--helper
  (expand-file-name "../bin/diff-review-herdr"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Helper shipped beside this Lisp directory.")

(defconst diff-review-herdr--metadata "diff-review-herdr")

(defun diff-review-herdr--read (file)
  "Read JSON from FILE as hash tables and vectors."
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :null-object nil :false-object nil)))

(defun diff-review-herdr--write (file data &optional pretty)
  "Atomically write DATA to JSON FILE, formatting when PRETTY.
Batch files are private; existing review permissions are preserved."
  (make-directory (file-name-directory file) t)
  (let ((temporary (make-temp-file (concat file "."))))
    (unwind-protect
        (progn
          (with-temp-file temporary
            ;; json-serialize returns UTF-8 bytes, not a multibyte Lisp string.
            (insert (decode-coding-string (json-serialize data :null-object nil) 'utf-8))
            (when pretty (json-pretty-print-buffer))
            (insert "\n"))
          (set-file-modes temporary (or (and pretty (file-modes file)) #o600))
          (rename-file temporary file t))
      (when (file-exists-p temporary) (delete-file temporary)))))

(defun diff-review-herdr--write-review ()
  "Write the current review atomically without corrupting UTF-8 strings.
Keep diff-review's write-contents-functions return value and buffer state."
  (diff-review-herdr--write (file-truename buffer-file-name) diff-review--data t)
  (set-visited-file-modtime)
  (set-buffer-modified-p nil)
  t)

;; The installed diff-review writer inserts json-serialize's unibyte UTF-8
;; result directly into a multibyte buffer before pretty printing.  Decode it
;; explicitly so replies remain intact on subsequent human edits as well.
(advice-add 'diff-review--write :override #'diff-review-herdr--write-review)

(defun diff-review-herdr--object (&rest pairs)
  "Make a JSON object from alternating key/value PAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs (puthash (pop pairs) (pop pairs) table))
    table))

(defun diff-review-herdr--review ()
  "Return the current review buffer, taking newer disk changes first."
  (let ((buffer (or (diff-review--review-buffer)
                    (user-error "Open a diff-review review first"))))
    (with-current-buffer buffer
      (when (buffer-modified-p)
        (user-error "Save the review JSON before sending feedback"))
      (unless (verify-visited-file-modtime) (diff-review--revert)))
    buffer))

(defun diff-review-herdr--metadata ()
  "Return this review's integration metadata, creating its table."
  (or (gethash diff-review-herdr--metadata diff-review--data)
      (let ((table (make-hash-table :test #'equal)))
        (puthash diff-review-herdr--metadata table diff-review--data)
        table)))

(defun diff-review-herdr--target ()
  "Resolve this review to a writable local worktree and its branch."
  (when (file-remote-p diff-review--root)
    (user-error "This bridge currently supports local reviews only"))
  (let* ((head (gethash "head" diff-review--data))
         (worktrees (diff-review--worktrees diff-review--root))
         (matches (seq-filter
                   (lambda (tree)
                     (if head
                         (equal head (plist-get tree :branch))
                       (file-equal-p diff-review--root (plist-get tree :path))))
                   worktrees)))
    (unless (and (= (length matches) 1) (plist-get (car matches) :branch))
      (user-error "Review a branch checked out in one writable worktree"))
    (let ((tree (car matches)))
      (diff-review-herdr--object
       "worktree" (file-truename (plist-get tree :path))
       "branch" (plist-get tree :branch)
       "head" head
       "common_dir" (diff-review--common-dir diff-review--root)
       "tip" (diff-review--git-at (plist-get tree :path) "rev-parse" "HEAD")))))

(defun diff-review-herdr--same-repository-p (agent common-dir)
  "Whether AGENT's working directory belongs to COMMON-DIR."
  (when-let* ((cwd (alist-get 'cwd agent)))
    (and (not (file-remote-p cwd)) (file-directory-p cwd)
         (ignore-errors
           (file-equal-p common-dir (diff-review--common-dir cwd))))))

(defun diff-review-herdr--agents (target)
  "Fetch the live Herdr agents belonging to TARGET's repository."
  (require 'herdr)
  (let ((snapshot (herdr--call-json-sync "api" "snapshot")))
    (unless snapshot (user-error "Cannot read the Herdr session"))
    (seq-filter
     (lambda (agent)
       (diff-review-herdr--same-repository-p agent (gethash "common_dir" target)))
     (herdr--extract-agents snapshot))))

(defun diff-review-herdr--pick-agent (target)
  "Ask for an agent belonging to TARGET's repository."
  (let* ((agents (herdr--sort-agents (diff-review-herdr--agents target)))
         (choices (mapcar (lambda (agent)
                            (cons (herdr--format-candidate agent) agent))
                          agents)))
    (unless choices (user-error "No Herdr agents in this repository"))
    (cdr (assoc (completing-read "Review agent: " choices nil t) choices))))

(defun diff-review-herdr--binding (agent target)
  "Describe AGENT and TARGET for a persistent review binding."
  (diff-review-herdr--object
   "pane" (alist-get 'pane-id agent) "name" (alist-get 'name agent)
   "kind" (alist-get 'kind agent) "session" (getenv "HERDR_SESSION")
   "worktree" (gethash "worktree" target) "branch" (gethash "branch" target)))

(defun diff-review-herdr--save-binding (agent target)
  "Save the binding of AGENT to TARGET in this review."
  (diff-review--modify-comments
   (lambda ()
     (puthash "binding" (diff-review-herdr--binding agent target)
              (diff-review-herdr--metadata)))))

(defun diff-review-herdr--active-batch ()
  "Return (FILE . DATA) for this review's unfinished batch, or nil."
  (when-let* ((file (gethash "batch" (diff-review-herdr--metadata))))
    (unless (file-exists-p file)
      (user-error "Feedback batch is missing: %s" file))
    (let ((batch (diff-review-herdr--read file)))
      (unless (member (gethash "state" batch) '("complete" "cancelled"))
        (cons file batch)))))

;;;###autoload
(defun diff-review-herdr-bind-agent ()
  "Associate this review with a live Herdr agent in the same repository."
  (interactive)
  (with-current-buffer (diff-review-herdr--review)
    (when (diff-review-herdr--active-batch)
      (user-error "Finish or cancel the outstanding feedback batch first"))
    (let* ((target (diff-review-herdr--target))
           (agent (diff-review-herdr--pick-agent target)))
      (diff-review-herdr--save-binding agent target)
      (message "Review bound to %s" (herdr--format-candidate agent)))))

(defun diff-review-herdr--bound-agent (target rebind)
  "Return the live agent bound to TARGET, asking when REBIND or unbound."
  (let* ((binding (gethash "binding" (diff-review-herdr--metadata)))
         (agent
          (unless (or rebind (null binding))
            (seq-find
             (lambda (candidate)
               (and (equal (gethash "pane" binding) (alist-get 'pane-id candidate))
                    (equal (gethash "kind" binding) (alist-get 'kind candidate))
                    (equal (gethash "name" binding) (alist-get 'name candidate))
                    (equal (gethash "session" binding) (getenv "HERDR_SESSION"))
                    (equal (gethash "branch" binding) (gethash "branch" target))
                    (file-equal-p (gethash "worktree" binding)
                                  (gethash "worktree" target))))
             (diff-review-herdr--agents target)))))
    (when (and binding (not rebind) (not agent))
      (user-error "Bound Herdr agent or worktree changed; use C-u to select again"))
    (or agent
        (let ((selected (diff-review-herdr--pick-agent target)))
          (diff-review-herdr--save-binding selected target)
          selected))))

(defun diff-review-herdr--fingerprint (comment)
  "Identify the content and conversation of COMMENT at dispatch time."
  (secure-hash
   'sha256
   (json-serialize
    (vector
     (vconcat (mapcar (lambda (key) (gethash key comment))
                      '("id" "path" "lines" "blob" "author" "at" "body" "resolved")))
     (vconcat (mapcar (lambda (reply)
                        (vconcat (mapcar (lambda (key) (gethash key reply))
                                         '("author" "at" "body"))))
                      (append (gethash "replies" comment) nil))))
    :null-object nil)))

(defun diff-review-herdr--client ()
  "Ensure a local Emacs server and return its client command."
  (require 'server)
  (let ((client (or (executable-find "emacsclient")
                    (user-error "emacsclient is not on Emacs's PATH"))))
    (unless (and (processp server-process) (process-live-p server-process))
      (when (server-running-p server-name)
        (user-error "Another Emacs owns server %s; choose a distinct server-name" server-name))
      (server-start))
    (vconcat
     (list client "--alternate-editor=false")
     (if server-use-tcp
         (list "--server-file" (expand-file-name server-name server-auth-dir))
       (list "--socket-name" (expand-file-name server-name server-socket-dir))))))

(defun diff-review-herdr--prompt (file batch)
  "Build the agent prompt for feedback BATCH stored in FILE."
  (let ((helper (mapconcat #'shell-quote-argument
                           (list diff-review-herdr--helper "--batch" file) " ")))
    (concat
     "Address this local diff-review feedback batch.\n"
     "Work only in: " (gethash "worktree" batch) "\n"
     "Expected branch: " (gethash "branch" batch) "\n"
     "Review JSON: " (gethash "review" batch) "\n"
     "Batch JSON (contains original comments and reply history): " file "\n\n"
     "Use explicit working directories even if your session started elsewhere. "
     "Verify the branch before editing. Follow the existing test and commit policy. "
     "These are the submitted thread IDs; do not process newer comments.\n\n"
     (mapconcat
      (lambda (thread)
        (let ((comment (gethash "comment" thread)))
          (format "[%s] %s %s\n%s"
                  (gethash "id" comment) (or (gethash "path" comment) "Review")
                  (or (gethash "lines" comment) "") (gethash "body" comment))))
      (append (gethash "threads" batch) nil) "\n\n")
     "\n\nAfter addressing EACH thread, report its result with the helper:\n"
     helper " resolve THREAD_ID --body 'What changed; checks and commit, if any'\n"
     "For disagreement or blocked work, reply and leave the thread open:\n"
     helper " reply THREAD_ID --body 'Reason or question'\n"
     "For multiline replies, use --body-file PATH instead of --body. "
     "Check that the helper succeeds. Do not edit the review JSON yourself, "
     "and do not approve the review. If reporting fails, state that explicitly. "
     "Send final results after any commits you make, so the review can refresh.\n")))

(defun diff-review-herdr--delivery-finished (file result)
  "Record submission RESULT for feedback FILE without inferring completion."
  (let ((batch (diff-review-herdr--read file)))
    (puthash "delivery" (if result "accepted" "unconfirmed") batch)
    (diff-review-herdr--write file batch)
    (message (if result "Feedback submitted to Herdr; waiting for thread replies"
               "Herdr delivery unconfirmed; inspect the agent before cancelling or retrying"))))

;;;###autoload
(defun diff-review-herdr-send (&optional rebind)
  "Send unresolved threads to the bound Herdr agent.
With prefix REBIND, choose a different agent before submitting.  This is
an explicit batch action; `diff-review-request-changes' remains unchanged."
  (interactive "P")
  (with-current-buffer (diff-review-herdr--review)
    (when (diff-review-herdr--active-batch)
      (user-error "Feedback is outstanding; use diff-review-herdr-status"))
    (unless (file-executable-p diff-review-herdr--helper)
      (user-error "Review helper is missing or not executable: %s" diff-review-herdr--helper))
    (let* ((target (diff-review-herdr--target))
           (comments (seq-remove (lambda (c) (gethash "resolved" c)) diff-review--comments)))
      (when (seq-empty-p comments) (user-error "No unresolved feedback to send"))
      (let* ((agent (diff-review-herdr--bound-agent target rebind))
             (status (alist-get 'status agent)))
        (unless (member status '("idle" "done"))
          (user-error "Herdr agent is %s; wait until it is ready for input" status))
        (let* ((client (diff-review-herdr--client))
               (directory (expand-file-name "review/.herdr/" (gethash "common_dir" target)))
               (file (progn (make-directory directory t)
                            (make-temp-file (expand-file-name "batch-" directory) nil ".json")))
               (batch (copy-hash-table target)))
          (puthash "format" "diff-review-herdr/1" batch)
          (puthash "id" (file-name-base file) batch)
          (puthash "review" (file-truename buffer-file-name) batch)
          (puthash "client" client batch)
          (puthash "state" "active" batch)
          (puthash "delivery" "submitting" batch)
          (puthash "agent" (diff-review-herdr--binding agent target) batch)
          (puthash "results" (make-hash-table :test #'equal) batch)
          (puthash "threads"
                   (vconcat (mapcar (lambda (comment)
                                      (diff-review-herdr--object
                                       "comment" comment
                                       "fingerprint" (diff-review-herdr--fingerprint comment)))
                                    (append comments nil))) batch)
          (diff-review-herdr--write file batch)
          (diff-review--modify-comments
           (lambda () (puthash "batch" file (diff-review-herdr--metadata))))
          (when (gethash "head" batch) (diff-review-request-changes))
          (condition-case err
              (herdr--call-async
               (lambda (result) (diff-review-herdr--delivery-finished file result))
               "agent" "prompt" (alist-get 'pane-id agent)
               (diff-review-herdr--prompt file batch))
            (error
             (diff-review-herdr--delivery-finished file nil)
             (signal (car err) (cdr err))))
          (message "Submitting %d review threads to Herdr" (length comments)))))))

(defun diff-review-herdr--validate-target (batch)
  "Check that BATCH still addresses this review's original worktree."
  (let ((target (diff-review-herdr--target)))
    (unless (and (equal (gethash "head" target) (gethash "head" batch))
                 (equal (gethash "branch" target) (gethash "branch" batch))
                 (file-equal-p (gethash "worktree" target) (gethash "worktree" batch))
                 (file-equal-p (gethash "common_dir" target) (gethash "common_dir" batch)))
      (user-error "The reviewed branch or worktree changed; result was not applied"))))

(defun diff-review-herdr-apply-result (encoded)
  "Apply a helper result ENCODED as base64 UTF-8 JSON.
This entry point is called by emacsclient, never by terminal scraping."
  (let* ((request (json-parse-string
                   (decode-coding-string (base64-decode-string encoded) 'utf-8)
                   :null-object nil :false-object nil))
         (file (gethash "batch" request))
         (batch (diff-review-herdr--read file))
         (id (gethash "thread" request))
         (action (gethash "action" request))
         (body (gethash "body" request))
         (thread (seq-find (lambda (item) (equal id (gethash "id" (gethash "comment" item))))
                           (gethash "threads" batch))))
    (unless (and (equal (gethash "format" batch) "diff-review-herdr/1")
                 thread (member action '("resolve" "reply"))
                 (stringp body) (not (string-empty-p (string-trim body))))
      (user-error "Invalid feedback result"))
    (when (equal (gethash "state" batch) "cancelled")
      (user-error "This feedback batch was cancelled"))
    (with-current-buffer (or (find-buffer-visiting (gethash "review" batch))
                            (find-file-noselect (gethash "review" batch)))
      (when (buffer-modified-p) (user-error "Review JSON has unsaved edits"))
      (unless (derived-mode-p 'diff-review-mode) (diff-review-mode))
      (diff-review--modify-comments
       (lambda ()
         (diff-review-herdr--validate-target batch)
         (let* ((comment (or (diff-review--comment id)
                             (user-error "Review thread %s was deleted" id)))
                (previous (seq-find
                           (lambda (reply)
                             (equal (gethash "diff-review-herdr-batch" reply)
                                    (gethash "id" batch)))
                           (gethash "replies" comment))))
           (if previous
               (unless (and (equal body (gethash "body" previous))
                            (equal action (gethash "diff-review-herdr-action" previous)))
                 (user-error "This thread already has a different result for this batch"))
             (unless (equal (gethash "fingerprint" thread)
                            (diff-review-herdr--fingerprint comment))
               (user-error "Thread %s changed since submission; result was not applied" id))
             (let ((reply (diff-review--message
                           (format "herdr/%s" (gethash "kind" (gethash "agent" batch))) body)))
               (puthash "diff-review-herdr-batch" (gethash "id" batch) reply)
               (puthash "diff-review-herdr-action" action reply)
               (puthash "replies" (vconcat (gethash "replies" comment) (vector reply)) comment)
               (when (equal action "resolve") (puthash "resolved" t comment)))))))
      (puthash id action (gethash "results" batch))
      (when (= (hash-table-count (gethash "results" batch)) (length (gethash "threads" batch)))
        (puthash "state" "complete" batch))
      (diff-review-herdr--write file batch)
      (diff-review-refresh)
      (message "Herdr replied to %s%s" id (if (equal action "resolve") " and resolved it" "")))
    "OK"))

;;;###autoload
(defun diff-review-herdr-status ()
  "Show the binding and most recent feedback batch for this review."
  (interactive)
  (with-current-buffer (diff-review-herdr--review)
    (let* ((metadata (diff-review-herdr--metadata))
           (binding (gethash "binding" metadata))
           (file (gethash "batch" metadata))
           (batch (and file (file-exists-p file) (diff-review-herdr--read file))))
      (with-help-window "*diff-review Herdr*"
        (princ (format "Agent: %s (%s)\nWorktree: %s\n"
                       (or (and binding (gethash "name" binding))
                           (and binding (gethash "pane" binding)) "unbound")
                       (or (and binding (gethash "kind" binding)) "")
                       (or (and binding (gethash "worktree" binding)) "unbound")))
        (if batch
            (progn
              (princ (format "Batch: %s\nState: %s\nDelivery: %s\n\n"
                             file (gethash "state" batch) (gethash "delivery" batch)))
              (seq-doseq (thread (gethash "threads" batch))
                (let* ((comment (gethash "comment" thread)) (id (gethash "id" comment)))
                  (princ (format "%s  %-8s  %s\n" id
                                 (or (gethash id (gethash "results" batch)) "pending")
                                 (diff-review--first-line comment))))))
          (princ "No feedback batch has been sent.\n"))
        (princ "\nUse diff-review-herdr-cancel from the review to stop tracking a batch.\n")))))

;;;###autoload
(defun diff-review-herdr-cancel ()
  "Stop accepting results for the outstanding batch.
This does not interrupt the agent or retract a submitted prompt."
  (interactive)
  (with-current-buffer (diff-review-herdr--review)
    (pcase (diff-review-herdr--active-batch)
      (`(,file . ,batch)
       (puthash "state" "cancelled" batch)
       (diff-review-herdr--write file batch)
       (message "Batch cancelled; late results will be rejected; the agent keeps running"))
      (_ (user-error "No outstanding feedback batch")))))

(keymap-set diff-review-mode-map "S" #'diff-review-herdr-send)
(keymap-set diff-review-mode-map "B" #'diff-review-herdr-bind-agent)
(keymap-set diff-review-mode-map "I" #'diff-review-herdr-status)
(keymap-set diff-review-file-command-map "s" #'diff-review-herdr-send)
(keymap-set diff-review-file-command-map "b" #'diff-review-herdr-bind-agent)
(keymap-set diff-review-file-command-map "i" #'diff-review-herdr-status)

(with-eval-after-load 'evil
  (when diff-review-evil-keys
    (dolist (map (list diff-review-mode-map diff-review-sidebar-mode-map))
      (evil-define-key* 'normal map
        "S" #'diff-review-herdr-send
        "B" #'diff-review-herdr-bind-agent
        "I" #'diff-review-herdr-status))))

(provide 'diff-review-herdr)
;;; diff-review-herdr.el ends here
