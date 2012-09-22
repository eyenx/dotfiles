export HISTFILE=~/.hist
export HISTSIZE=5000
export SAVEHIST=20000
export CORRECT_IGNORE='_*'
#source zshenv
source ~/.zshenv
#some options
setopt appendhistory autocd extendedhistory histignorespace histignorealldups histreduceblanks histverify correctall extendedglob bashautolist
#vi...
bindkey -v
autoload -Uz edit-command-line zmv 
zle -N edit-command-line
bindkey -M vicmd 'v' edit-command-line
#zstyle
#descr
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:warnings' format 'no matches: %d'
zstyle ':completion:*' verbose yes
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*' group-name  ''
#menu
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
#procs
zstyle ':completion:*:processes' command 'ps ax -o pid,stime,args | sed "/ps/d"'
#compdef for pacman
compdef _pacman pacman-color=pacman
compdef _pacman packer=pacman
#alias file
source ~/.aliases
#functs file
#autoload .functs
source ~/.functs
source ~/.mailfuncts
#fasd
eval "$(eval fasd --init auto)"
#grep color
export GREP_COLOR='1;31'
