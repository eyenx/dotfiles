umask 0077
PATH=$PATH:$HOME/bin
export PATH
export EDITOR=vim
export BROWSER=firefox
export GTK2_RC_FILES="/etc/gtk-2.0/gtkrc:$HOME/.gtkrc-2.0"
#export TERM="rxvt-unicode"
# colored man
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

export KAKI='dini mami'
#jp2a if tty

#if [[ $(echo $TTY|cut -c 6-8) == "tty" ]]; then
#	toilet -f mono12 -F metal -F border "welcome"
#	jp2a  --height=50 ~/img/faces/face4.jpg	
#fi
