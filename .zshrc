# Path to your oh-my-zsh configuration.
ZSH=$HOME/.omzsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
ZSH_THEME="eye"

# Uncomment following line if you want to disable autosetting terminal title.
if [[ $(echo $TTY | cut -c 6-8) == "tty" ]]; then
	DISABLE_AUTO_TITLE="true"
fi

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(vi-mode tig git github extract archlinux python perl svn encode64 perl urltools systemd catimg docker docker-compose kubectl ansible oc fzf vault taskwarrior forgit-git)
source $ZSH/oh-my-zsh.sh
#temporary fix
unset GREP_OPTIONS

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/bin/vault vault
