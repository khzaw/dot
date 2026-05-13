# Dotfiles

Portable dotfiles for macOS and Linux. Common config is tracked here; secrets,
work-only settings, and host-specific overrides live in untracked local files.

## Install

Preview links:

```sh
./bootstrap/install --dry-run
```

Replace existing files with backups:

```sh
./bootstrap/install --force
```

## Local Files

Loaded by zsh, in this order:

```text
~/.config/dot/env.zsh
~/.config/dot/secrets.zsh
~/.config/dot/work.zsh
~/.config/dot/hosts/<hostname>.zsh
~/Code/dot/zsh/*.local
```

Use these for tokens, work aliases, Vault variables, machine-only paths, and
anything that should not be shared across every device.

## Shell

The shell stack is:

- Antidote for zsh plugin management
- zsh-completions, autosuggestions, fast syntax highlighting, history search
- optional `mise`, `zoxide`, `atuin`, and `starship` when installed
- file-based completion caching under `~/.cache/zsh`

`mise` is the preferred runtime manager. It covers Node plus Python, Go, Ruby,
Rust, project environments, and tasks. Volta can stay installed as a temporary
fallback on machines that have not migrated yet.

## Versioning Rule

Track hand-written source/config. Do not track caches, sessions, auth, history,
tokens, app UI state, generated package installs, or per-machine work config.
