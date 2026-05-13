# AI Harnesses

Shared AI tool config lives here. Auth, history, session databases, generated
caches, and trusted project lists stay local.

Suggested links:

```sh
ln -s ~/Code/dot/ai/codex/config.toml ~/.codex/config.toml
ln -s ~/Code/dot/ai/codex/rules/default.rules ~/.codex/rules/default.rules
ln -s ~/Code/dot/ai/opencode/opencode.json ~/.config/opencode/opencode.json
ln -s ~/Code/dot/ai/opencode/dcp.jsonc ~/.config/opencode/dcp.jsonc
ln -s ~/Code/dot/ai/eca/config.json ~/.config/eca/config.json
ln -s ~/Code/dot/ai/zed/settings.json ~/.config/zed/settings.json
```

Keep machine-local overrides in `~/.config/dot/ai/`.
