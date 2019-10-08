# Path to your oh-my-zsh configuration.
ZSH=$HOME/.omzsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#if [[ $(echo $TTY|cut -c 6-8) == "tty" ]]; then
#	ZSH_THEME="eye_`hostname`"
#else
ZSH_THEME="eye"
#fi

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
if [[ $(echo $TTY | cut -c 6-8) == "tty" ]]; then
	DISABLE_AUTO_TITLE="true"
fi

# Uncomment following line if you want red dots to be displayed while waiting for completion
#COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(vi-mode tig git github extract archlinux python perl svn encode64 perl urltools systemd catimg docker docker-compose kubectl ansible oc zsh-navigation-tools fasd)
source $ZSH/oh-my-zsh.sh
#temporary fix
unset GREP_OPTIONS

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /home/eye/dev/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . /home/eye/dev/serverless/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /home/eye/dev/serverless/node_modules/tabtab/.completions/sls.zsh ]] && . /home/eye/dev/serverless/node_modules/tabtab/.completions/sls.zsh


# add Pulumi to the PATH
export PATH=$PATH:$HOME/.pulumi/bin
