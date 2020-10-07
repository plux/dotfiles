# ------------------------------------------------------------------------------
#          FILE:  plux.zsh-theme
#   DESCRIPTION:  oh-my-zsh theme file.
#        AUTHOR:  Plux
#       VERSION:  1.1.0
#    SCREENSHOT:
# ------------------------------------------------------------------------------


if [[ "$TERM" != "dumb" ]] && [[ "$DISABLE_LS_COLORS" != "true" ]]; then
    PROMPT='[%{$fg[green]%}%~%{$reset_color%}] %{$fg_bold[grey]%}%*%{$reset_color%}
%# '

    ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[grey]%}"
    ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
    ZSH_THEME_GIT_PROMPT_DIRTY=""
    ZSH_THEME_GIT_PROMPT_CLEAN=""

    # display exitcode on the right when >0
    return_code="%(?..%{$fg[red]%} %?%{$reset_color%})"

    RPROMPT='$(git_prompt_info)$(git_prompt_status)${return_code}%{$reset_color%}'

    ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[green]%} +"
    ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg[blue]%} m"
    ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[red]%} x"
    ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg[magenta]%} r"
    ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[yellow]%} ="
    ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[cyan]%} ?"
else
    PROMPT='%~ $(git_prompt_info)
%# '

    ZSH_THEME_GIT_PROMPT_PREFIX="["
    ZSH_THEME_GIT_PROMPT_SUFFIX="]"
    ZSH_THEME_GIT_PROMPT_DIRTY=""
    ZSH_THEME_GIT_PROMPT_CLEAN=""

    # display exitcode on the right when >0
    return_code="%(?.. %?)"

    RPROMPT='$(git_prompt_info) ${return_code}$(git_prompt_status)'

    ZSH_THEME_GIT_PROMPT_ADDED=" +"
    ZSH_THEME_GIT_PROMPT_MODIFIED=" m"
    ZSH_THEME_GIT_PROMPT_DELETED=" x"
    ZSH_THEME_GIT_PROMPT_RENAMED=" r"
    ZSH_THEME_GIT_PROMPT_UNMERGED=" ="
    ZSH_THEME_GIT_PROMPT_UNTRACKED=" ?"
fi
