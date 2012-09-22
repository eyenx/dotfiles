PROMPT=$'%{$reset_color%}┌─┤%B%{$fg[blue]%}%n%b%{$fg[blue]%}_%M%{$reset_color%}│ %~\n└─╼ '
RPROMPT='$(vi_mode_prompt_info)$(git_prompt_info)$(qload)'
# │ %{$fg[blue]%}%T%{$reset_color%}'

## GIT

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[blue]%}(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} │ "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[red]%}✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"

## VI

MODE_INDICATOR="%{$fg[blue]%}<%{$fg[red]%}v%{$reset_color%} "


