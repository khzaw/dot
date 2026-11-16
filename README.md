# Dotfiles

Emacs supports [sending diff-review feedback to Herdr agents](emacs.d/diff-review-herdr.md).

## Machine-local settings

Shared application defaults live in the tracked config files. Values that
depend on a machine's display should live in an ignored `config.local` file.

Ghostty loads `config/ghostty/config.local` after its shared config. For
example:

```ini
font-size = 10.5
```
