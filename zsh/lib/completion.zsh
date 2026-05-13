# Keep completions file-based. Avoid generating completions with subprocesses on
# every shell startup; install them into ~/.zfunc instead.
zstyle ':completion:*:*:git:*' script "$HOME/.zfunc/git-completion.bash"
