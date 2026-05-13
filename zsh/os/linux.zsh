if [[ -n "$WAYLAND_DISPLAY" ]]; then
  command -v wl-copy >/dev/null 2>&1 && alias pbcopy='wl-copy'
  command -v wl-paste >/dev/null 2>&1 && alias pbpaste='wl-paste'
elif [[ -n "$DISPLAY" ]]; then
  command -v xclip >/dev/null 2>&1 && alias pbcopy='xclip -selection clipboard'
  command -v xclip >/dev/null 2>&1 && alias pbpaste='xclip -selection clipboard -o'
fi

path=(
  "$HOME/.opencode/bin"
  "$HOME/.gobrew/current/bin"
  "$HOME/.gobrew/bin"
  "$HOME/.cargo/bin"
  $path
)

export GOPATH="${GOPATH:-$HOME/.gobrew/current/go}"
