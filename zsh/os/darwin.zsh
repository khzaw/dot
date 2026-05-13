if [[ -x /opt/homebrew/bin/brew ]]; then
  export HOMEBREW_PREFIX="${HOMEBREW_PREFIX:-/opt/homebrew}"
elif [[ -x /usr/local/bin/brew ]]; then
  export HOMEBREW_PREFIX="${HOMEBREW_PREFIX:-/usr/local}"
fi

if [[ -n "$HOMEBREW_PREFIX" ]]; then
  path=(
    "$HOMEBREW_PREFIX/bin"
    "$HOMEBREW_PREFIX/sbin"
    $path
  )
fi

export ANDROID_HOME="${ANDROID_HOME:-$HOME/Library/Android/sdk}"

path=(
  "$HOME/Library/Application Support/JetBrains/Toolbox/scripts"
  "$HOME/.opencode/bin"
  "$HOME/.gobrew/current/bin"
  "$HOME/.gobrew/bin"
  "$HOME/.cargo/bin"
  "$ANDROID_HOME/platform-tools"
  "$ANDROID_HOME/tools"
  $path
)

alias localip='ipconfig getifaddr en1'
alias flush='dscacheutil -flushcache; sudo killall -HUP mDNSResponder'
alias show='defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder'
alias hide='defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder'
alias afk='/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend'

command -v md5sum >/dev/null 2>&1 || alias md5sum='md5'
command -v sha1sum >/dev/null 2>&1 || alias sha1sum='shasum'
