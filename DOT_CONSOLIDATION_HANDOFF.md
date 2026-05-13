# Dot Consolidation Handoff

This document is the working handoff for continuing the dotfiles consolidation
from a fresh agent session.

## Current State

- Repo: `/home/khz/Code/dot`
- Branch: `dot-consolidation-zsh`
- Base branch was clean before work started.
- Goal: make this one dotfiles repo usable across multiple personal macOS
  machines, one work macOS machine, and an Arch Linux machine.
- Key constraints:
  - Common config should be versioned.
  - Secrets, work-only vars, and host-specific hacks must stay untracked.
  - zsh startup should stay fast.
  - Emacs, Git, terminal, AI harnesses, and app configs all belong under one
    coherent repo policy.

## Decisions Made

### Runtime Manager

Prefer `mise` over Volta for the consolidated setup.

Reasoning:

- Volta is excellent for Node-specific runtime and package executable pinning.
- This repo needs cross-language runtime management across macOS/Linux and
  personal/work machines.
- `mise` covers Node plus Python, Go, Ruby, Rust, project env vars, and tasks.
- Volta can remain installed temporarily on machines that are not migrated yet.

Tracked config added:

- `config/mise/config.toml`
- `config/mise/README.md`

### Zsh Stack

Use Antidote instead of Antigen.

Tracked shell structure now aims for:

- `zsh/zshenv`: minimal env/XDG/path/local-secret loading.
- `zsh/zshrc`: interactive shell setup, Antidote, completion init, optional
  `mise`, `zoxide`, `atuin`, `starship`.
- `zsh/plugins.txt`: Antidote bundle list.
- `zsh/lib/*.zsh`: common shell library files.
- `zsh/os/linux.zsh`, `zsh/os/darwin.zsh`: OS-specific setup.
- `zsh/local.example.zsh`: examples for untracked local files.

Local files are loaded from:

- `~/.config/dot/env.zsh`
- `~/.config/dot/secrets.zsh`
- `~/.config/dot/work.zsh`
- `~/.config/dot/hosts/<hostname>.zsh`
- `~/Code/dot/zsh/*.local`

### Secrets

Revoked token literals were removed from tracked files.

Important removals:

- `zsh/zshenv.linux` no longer contains `GITHUB_TOKEN`.
- `emacs.d/lisp/secrets.el` was deleted from the tracked tree.
- `emacs.d/init.el` now optionally loads:

```elisp
(load (expand-file-name "local/secrets.el" user-emacs-directory) t)
```

Use `emacs.d/local/secrets.el` for real local Emacs secrets.

### AI Harnesses

Added a safe shared `ai/` area:

- `ai/codex/config.toml`
- `ai/codex/rules/default.rules`
- `ai/opencode/opencode.json`
- `ai/opencode/dcp.jsonc`
- `ai/eca/config.json`
- `ai/zed/settings.json`
- `ai/README.md`

Do not version:

- `~/.codex/auth.json`
- history files
- SQLite logs/state
- trusted project lists if they are machine-specific
- provider API keys

## Implemented Changes

### Added

- `DOT_CONSOLIDATION_HANDOFF.md`
- `README.md` expanded with install/local/versioning rules.
- `bootstrap/install`
- `config/mise/config.toml`
- `config/mise/README.md`
- `ai/`
- `zsh/plugins.txt`
- `zsh/lib/aliases.zsh`
- `zsh/lib/functions.zsh`
- `zsh/lib/completion.zsh`
- `zsh/os/linux.zsh`
- `zsh/os/darwin.zsh`
- `zsh/local.example.zsh`
- `emacs.d/lisp/secrets.example.el`

### Modified

- `.gitignore`
  - ignores local zsh files, `config/dot/`, `emacs.d/local/`, and
    `emacs.d/lisp/secrets.el`.
- `zsh/zshenv`
  - rebuilt as minimal portable env setup.
- `zsh/zshrc`
  - rebuilt around Antidote and optional modern tools.
- `zsh/aliases.zsh`
  - heavily pruned from old large OMZ-style alias dump.
- `zsh/functions`
  - heavily pruned and modernized.
- `zsh/zshenv.linux`, `zsh/zshenv.mac`, `zsh/zshrc.linux`, `zsh/zshrc.mac`
  - replaced with backward-compatible shims.
- `emacs.d/init.el`
  - now optional-loads local secrets.
- `emacs.d/config.org`
  - Spotify credentials replaced with variables.
- `emacs.d/lisp/init-ui.el`
  - Linux theme-switch font-size bug patched.

### Deleted

- `emacs.d/lisp/secrets.el`

## Validation Already Done

Ran:

```sh
zsh -n zsh/zshenv zsh/zshrc zsh/aliases.zsh zsh/functions zsh/os/linux.zsh zsh/os/darwin.zsh bootstrap/install
git diff --check
```

Both passed.

Temp `ZDOTDIR` startup test for the new repo zsh:

```sh
tmp=$(mktemp -d)
ln -s /home/khz/Code/dot/zsh/zshenv "$tmp/.zshenv"
ln -s /home/khz/Code/dot/zsh/zshrc "$tmp/.zshrc"
for i in 1 2 3 4 5; do
  time env ZDOTDIR=$tmp HOME=/home/khz DOTFILES_DIR=/home/khz/Code/dot zsh -i -c exit
done
rm -rf "$tmp"
```

Observed warm startup around `76-79ms`.

Secret scan no longer finds concrete revoked tokens. Remaining matches are
placeholders/examples or false positives.

Emacs paren check for `init-ui.el` passed:

```sh
emacs --batch -Q --eval '(with-temp-buffer (insert-file-contents "emacs.d/lisp/init-ui.el") (emacs-lisp-mode) (check-parens))'
```

## Known Follow-Up Work

### 1. Review Alias and Function Pruning

Files:

- `zsh/aliases.zsh`
- `zsh/functions`

The old files were very large. The new versions keep only high-signal common
aliases/functions. Ask the user whether anything from the old alias dump is
still wanted.

Likely restore candidates:

- some less-used Git aliases
- macOS Finder helpers
- archive/data URL/cert helpers

Keep machine-specific or rarely used functions out of common startup if they
are not needed everywhere.

### 2. Adopt Zsh on This Linux Machine

Current active Linux shell files still exist outside the repo:

- `~/.zshenv`
- `~/.zshrc`
- `~/.zsh_plugins.txt`

The bootstrap dry-run currently skips them because they exist.

Recommended path:

1. Move local-only active values into:
   - `~/.config/dot/secrets.zsh`
   - `~/.config/dot/work.zsh`
   - `~/.config/dot/hosts/<hostname>.zsh`
2. Run:

```sh
./bootstrap/install --dry-run
```

3. If output looks right:

```sh
./bootstrap/install --force
```

Backups go under:

```text
~/.local/state/dot/backup/<timestamp>
```

### 3. Add `bootstrap/doctor`

Useful next implementation task.

It should report:

- current branch
- repo dirty status
- OS and hostname
- whether expected symlinks point into repo
- missing optional tools:
  - `zsh`
  - `antidote`
  - `mise`
  - `zoxide`
  - `atuin`
  - `starship`
  - `eza`
  - `bat`
  - `rg`
- zsh startup time
- whether local files exist:
  - `~/.config/dot/secrets.zsh`
  - `~/.config/dot/work.zsh`
  - `~/.config/dot/hosts/<hostname>.zsh`
- obvious tracked secret literals

### 4. Split Git Config

Current `git/gitconfig` still mixes shared and personal/machine-specific state.

Problems to fix:

- personal identity is common
- macOS credential helper is common
- GPG program path is macOS-specific
- `http.sslVerify = false` is unsafe as a common default
- work/personal identity should be conditional

Recommended structure:

```text
git/gitconfig
git/gitconfig.common
git/gitconfig.example-local
```

Or make `git/gitconfig` the common file and add:

```ini
[includeIf "gitdir:~/Code/work/"]
  path = ~/.config/dot/git/work.gitconfig

[includeIf "gitdir:~/Code/personal/"]
  path = ~/.config/dot/git/personal.gitconfig
```

Keep real local Git files untracked.

### 5. Finish AI Config Linking Policy

`bootstrap/install` links safe shared AI config into:

- `~/.codex/config.toml`
- `~/.codex/rules/default.rules`
- `~/.config/opencode/opencode.json`
- `~/.config/opencode/dcp.jsonc`
- `~/.config/eca/config.json`
- `~/.config/zed/settings.json`

Before using `--force`, review whether machine-specific Codex project trust
entries need to be preserved elsewhere.

Codex trusted project lists should probably stay local, not tracked.

### 6. Emacs Local Config Cleanup

Already fixed:

- tracked `secrets.el` removed
- local secrets optional-loaded
- `init-ui.el` theme-switch Linux font-size bug patched

Still review for work/private config:

- `emacs.d/lisp/init-jira.el`
- `emacs.d/lisp/init-slack.el`
- org paths in `init-org.el`
- private feeds in `elfeed.org`
- machine-specific font assumptions

Move local/private values into:

```text
emacs.d/local/*.el
```

### 7. Mac Rollout

Do not start with the work Mac.

Recommended rollout order:

1. Current Arch Linux machine.
2. One personal macOS machine.
3. Other personal macOS machines.
4. Work macOS machine last.

Each rollout should run:

```sh
./bootstrap/install --dry-run
```

Then inspect conflicts before using `--force`.

## Useful Commands

Current branch/status:

```sh
git status --short --branch
```

Review all changed files:

```sh
git diff --stat
git diff --name-status
```

Run zsh syntax check:

```sh
zsh -n zsh/zshenv zsh/zshrc zsh/aliases.zsh zsh/functions zsh/os/linux.zsh zsh/os/darwin.zsh bootstrap/install
```

Run whitespace check:

```sh
git diff --check
```

Dry-run bootstrap:

```sh
./bootstrap/install --dry-run
```

Secret scan:

```sh
rg -n "(ghp_[A-Za-z0-9_]+|sk-[A-Za-z0-9_-]+|GITHUB_TOKEN=.*[A-Za-z0-9]|TOKEN=.*[A-Za-z0-9]|SECRET=.*[A-Za-z0-9]|PASSWORD=.*[A-Za-z0-9]|spotify-secret \"|spotify-client-id \"|OPENAI_API_KEY=.*[A-Za-z0-9]|ANTHROPIC_API_KEY=.*[A-Za-z0-9]|GEMINI_API_KEY=.*[A-Za-z0-9])" \
  -g '!emacs.d/straight/**' \
  -g '!emacs.d/.cache/**' \
  -g '!emacs.d/eln-cache/**' \
  -g '!emacs.d/leetcode-env/**' \
  -g '!emacs.d/var/**'
```

## Current Open Question

The next agent should ask whether to continue with:

1. `bootstrap/doctor`
2. Git config split
3. shell adoption on this Arch machine
4. Emacs local/private cleanup
5. alias/function restoration pass

Best default next task: implement `bootstrap/doctor`, because it makes rollout
safer before applying symlinks to this machine or any Mac.
