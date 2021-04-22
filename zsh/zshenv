setopt no_global_rcs

# home
export HOME="/Users/$(whoami)"

# history
export HISTSIZE=32768
export HISTFILESIZE=$HISTSIZE
export HISTCONTROL=ignoredups
export HISTIGNORE="ls:cd:cd -:pwd:exit:date:* --help"

# lang
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export LDFLAGS="-L/usr/local/opt/openssl/lib -L/usr/local/opt/zlib/lib -L/usr/local/opt/sqlite/lib"
export CPPFLAGS="-I/usr/local/opt/openssl/include -L/usr/local/opt/zlib/include -L/usr/local/opt/sqlite/include"

# misc
export EDITOR="vim"
export GREP_OPTIONS="--color=auto"
# don't clear the screen after quitting a manual page
export MANPAGER="less -X"
# highlight section titles in manual pages
export LESS_TERMCAP_md="${yellow}"

# python
export PYENV_ROOT="$HOME/.pyenv"
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

# android
export ANDROID=$HOME/Library/Android/sdk
export ANDROID_HOME=$HOME/Library/Android/sdk
export ANDROID_SDK="$ANDROID"
export ANDROID_PLATFORM_TOOLS="$ANDROID_SDK/platform-tools"

# java
# export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)

# go
export GOPATH=$HOME/.go
export GO111MODULE=on
export PKG_CONFIG_PATH="/usr/local/opt/openssl/lib/pkgconfig"

# PATH
unset PATH
PATH="$HOME/bin"
PATH="$PATH:$PYENV_ROOT/shims"
PATH="$PATH:$HOME/.fastlane/bin"															# Fastlane
PATH="$PATH:/usr/texbin"                                     # LaTeX
PATH="$PATH:$HOME/.local/bin"                                # Pipsi
PATH="$PATH:/usr/local/opt/sqlite/bin"
PATH="$PATH:/usr/local/opt/imagemagick@6/bin"
PATH="$PATH:/usr/local/bin:/usr/local/sbin"                  # homebrew
PATH="$PATH:/usr/local/opt/openssl/bin"
PATH="$PATH:/usr/bin:/usr/sbin"
PATH="$PATH:$HOME/.cabal/bin"                                # Haskell
PATH="$PATH:$HOME/.rbenv"                                    # Ruby
PATH="$PATH:$HOME/Library/Android/sdk/tools"	                # Android
PATH="$PATH:$HOME/Library/Android/sdk/platform-tools"        # Android
PATH="$PATH:/usr/local/heroku/bin"                           # Heroku Toolbet
PATH="$PATH:/usr/local/go/bin"                               # Go
PATH="$PATH:/usr/local/opt/go/libexec/bin"                   # Go
PATH="$PATH:$GOPATH/bin"                                     # Go path
PATH="$PATH:/usr/local/opt/mysql/bin"                        # mysql
PATH="$PATH:/usr/local/smlnj/bin"                            # smlnj
PATH="$PATH:/usr/local/opt/coreutils/libexec/gnubin"         # GNU stuffs
PATH="$PATH:/bin:/sbin:/opt/X11/bin"                         # OSX built in
PATH="$PATH:/Developer/usr/bin"                              # Apple Developer
PATH="$PATH:/usr/local/Cellar/smlnj/110.84/libexec/bin"      # smlnj
PATH="$PATH:/usr/local/opt/openjdk/bin"
PATH="$PATH:/usr/local/opt/openssl@1.1/bin"
#export PATH="$PATH:/usr/local/opt/terraform@0.12/bin"               # terraform
export PATH="/Applications/Postgres.app/Contents/Versions/latest/bin:$PATH" # Postgres App

MANPATH="$MANPATH:/usr/local/share/man"
MANPATH="$MANPATH:/usr/share/man"
MANPATH="$MANPATH:/opt/X11/share/man"
MANPATH="$MANPATH:/Library/TeX/Distributions/.DefaultTeX/Contents/Man"
MANPATH="$MANPATH:/usr/local/opt/erlang/lib/erlang/man"
MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
export MANPATH="$MANPATH:/Library/TeX/Distributions/.DefaultTeX/Contents/Man"

export NVM_LAZY_LOAD=true
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f $HOME/.config/yarn/global/node_modules/tabtab/.completions/serverless.zsh ]] && . $HOME/.config/yarn/global/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f $HOME/.config/yarn/global/node_modules/tabtab/.completions/sls.zsh ]] && . $HOME/.config/yarn/global/node_modules/tabtab/.completions/sls.zsh

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
