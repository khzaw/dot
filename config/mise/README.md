# mise

`mise` is the preferred runtime manager for this dotfiles repo.

Use it for polyglot tool versions, project-local environment variables, and
repeatable tasks. Keep Volta only as a fallback on machines that have not been
migrated yet.

Recommended links:

```sh
ln -s ~/Code/dot/config/mise ~/.config/mise
```

Local secrets belong in untracked files such as:

```text
~/.config/dot/secrets.zsh
~/.config/dot/work.zsh
~/.config/dot/hosts/<hostname>.zsh
```
