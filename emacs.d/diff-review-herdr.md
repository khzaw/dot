# Local review feedback through Herdr

Open a branch with `M-x diff-review-branch`, add comments normally, then
press **S** in its overview or sidebar to send unresolved feedback to a Herdr
agent. From a file under review, use **C-c _ s** (or the equivalent under
your configured `diff-review-command-prefix`).

The first submission asks for an agent in the same Git repository. It remembers
the chosen pane and worktree in the review JSON. The agent may be running in
the main checkout or the feature worktree: every prompt names the actual
feature worktree explicitly. Later submissions recheck the pane, agent kind,
name, Herdr session, and worktree before reusing the binding. If you replace
an agent in the same pane, explicitly rebind the review to its new session.

| Command | Overview/sidebar | File review prefix |
| --- | --- | --- |
| Send unresolved feedback | `S` | `s` |
| Choose/rebind agent | `B` | `b` |
| Show batch status | `I` | `i` |

`C-u M-x diff-review-herdr-send` also selects an agent again. The existing
`C` / `diff-review-request-changes` command still only records a verdict.
Sending a branch review records that verdict too. The agent must be idle or
done before submission; the bridge does not queue prompts into working or
blocked sessions. Only local reviews are supported. A branch must have a
writable worktree checked out; fixed commit-range reviews are not editable
targets.

Manually collapsed sidebar folders stay closed when switching files or
redrawing the tree. Use `TAB` to expand them when desired. This fold state lasts
for the current sidebar buffer; it is not stored in the review JSON.

## Replies and resolution

The generated prompt includes the helper's absolute path, an absolute batch
file path, the submitted thread IDs, and instructions to report each result.
The helper takes either an inline reply or a file:

```sh
/path/to/emacs.d/bin/diff-review-herdr --batch /path/to/batch.json \
    resolve THREAD_ID --body 'Removed the documentation in abc123.'

/path/to/emacs.d/bin/diff-review-herdr --batch /path/to/batch.json \
    reply THREAD_ID --body-file /path/to/explanation.txt
```

`resolve` appends the explanation and marks the thread resolved. `reply`
appends an explanation or question and leaves it open. Either completes that
thread's entry in the submitted batch. After every submitted thread has a
result, another batch can be sent. New comments added during a batch are kept
for the next round. The agent still has to follow the reporting instructions;
its idle/done status never resolves a comment.

Emacs remains the review writer. The helper uses `emacsclient` to contact the
originating Emacs server, including a named server when configured. First send
starts that Emacs's local server if necessary. If another Emacs already owns
its configured server name, choose a distinct `server-name` before sending.
The helper requires Python 3; both Python and `emacsclient` must remain
available, and the originating Emacs must be running with this bridge loaded.

Results are rejected if the thread was edited, deleted, resolved, or replied
to after submission, or if the target branch/worktree changed. An identical
retry is idempotent, including after you reopen an already addressed thread.
Replies preserve diff-review's unknown JSON fields. The bridge also supplies
an atomic, UTF-8-safe review writer so subsequent human edits preserve Unicode
replies with the installed diff-review version. The review refreshes after
writeback; branch reviews show committed changes, while worktree reviews can
show uncommitted fixes. Sending does not impose a new commit or push policy on
agents.

## A batch is stuck or delivery is uncertain

Use `M-x diff-review-herdr-status` from the review to inspect delivery and
per-thread results. Delivery acknowledgement is separate from completion.
Unconfirmed deliveries are kept outstanding and are never automatically
retried: inspect the agent before sending the same feedback again.

`M-x diff-review-herdr-cancel` stops tracking an outstanding batch, allowing a
fresh submission. It does **not** interrupt the agent or retract its prompt;
results from the cancelled batch will be rejected. If feedback changed during
work, inspect the fix, then cancel and send an updated batch as appropriate.

Bindings live in the existing review JSON under `diff-review-herdr`. Private
batch snapshots and result bookkeeping live under the repository's common
Git directory at `review/.herdr/`. They are shared by worktrees and are not
committed to the project being reviewed. The helper returns a nonzero exit
status whenever writeback is unconfirmed; the agent should report that instead
of claiming resolution.

## Verification

With the configured Straight dependencies already installed, run:

```sh
emacs.d/tests/run-diff-review-herdr-tests
```

`EMACS` and `STRAIGHT_REPOS` can override the Emacs executable and installed
package source directory. The suite creates temporary Git worktrees, stubs
Herdr submission, and exercises the real Python helper against an isolated
Emacs server. It does not contact live Herdr agents or modify existing reviews.
