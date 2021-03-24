umask 0022
export PATH=$HOME/bin:$HOME/.cabal/bin:$HOME/bin/node_modules/.bin:$HOME/.gem/ruby/latest/bin:$PATH:~/.go/bin
export XDG_CONFIG_HOME="/home/eye/.config"
stty -ixon
export PATH
export XDG_CONFIG_HOME
export EDITOR=vim
export BROWSER=chromium
export GTK2_RC_FILES="/etc/gtk-2.0/gtkrc:$HOME/.gtkrc-2.0"
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"
if [[ -n $TMUX ]]
    then
        export TERM=screen-256color
    else
        export TERM=xterm-256color
fi
# colored man
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

export JAVA_HOME=/usr/lib/jvm/java-10-openjdk
export WHALEBREW_INSTALL_PATH=~/.whalebrew/bin
export GOPATH=$HOME/.go
