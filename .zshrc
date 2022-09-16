export GIT_MERGE_AUTOEDIT=no

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="plux"
# Good themes:
# bira
# candy
# fishy
# kphoen

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#

## Aliases
alias e="emacsclient -c -n"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

REPORTTIME=10

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)

plugins=(git gitfast pass history-substring-search fzf-tab)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games
export PATH=/home/hakan/.bin:$PATH
export PATH=/home/hakan/cisco/git-bootstrap/commands/:$PATH
export PATH=$HOME/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:$PATH
export PATH=$PATH:/home/hakan/.cache/rebar3/bin
export PATH=$PATH:/home/hakan/.cargo/bin
#export PATH=$PATH:/home/hakan/install/sbt/bin
#export PATH=$PATH:/home/hakan/install/spark-2.3.1-bin-hadoop2.7/bin
export EDITOR=emacsclient

# History tweaks
export HISTFILESIZE=1000000000
export HISTSIZE=1000000000

setopt SHARE_HISTORY
export HISTTIMEFORMAT="[%F %T] "

# history-substring-search options
setopt HIST_FIND_NO_DUPS

# Setup erlang
. /home/hakan/install/erl-24.1/activate

# Setup elixir
#test -s "$HOME/.kiex/scripts/kiex" && source "$HOME/.kiex/scripts/kiex"
#source $HOME/.kiex/elixirs/elixir-1.3.2.env

# ix
ix() {
    local opts
    local OPTIND
    [ -f "$HOME/.netrc" ] && opts='-n'
    while getopts ":hd:i:n:" x; do
        case $x in
            h) echo "ix [-d ID] [-i ID] [-n N] [opts]"; return;;
            d) $echo curl $opts -X DELETE ix.io/$OPTARG; return;;
            i) opts="$opts -X PUT"; local id="$OPTARG";;
            n) opts="$opts -F read:1=$OPTARG";;
        esac
    done
    shift $(($OPTIND - 1))
    [ -t 0 ] && {
        local filename="$1"
        shift
        [ "$filename" ] && {
            curl $opts -F f:1=@"$filename" $* ix.io/$id
            return
        }
        echo "^C to cancel, ^D to send."
    }
    curl $opts -F f:1='<-' $* ix.io/$id
}

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

zstyle -e ':completion::*:*:*:hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

unsetopt correct
unsetopt correctall
export ANSIBLE_CONFIG="~/kivra/infrastructure_ng/se/ansible.cfg"


#source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh

_fzf_complete_pass() {
    _fzf_complete '+m' "$@" < <(
        local pwdir=${PASSWORD_STORE_DIR-~/.password-store/}
        local stringsize="${#pwdir}"
        find "$pwdir" -name "*.gpg" -print |
            cut -c "$((stringsize + 1))"-  |
            sed -e 's/\(.*\)\.gpg/\1/'
    )
}

_fzf_complete_git() {
    ARGS="$@"
    local branches
    branches=$(git branch -vv --all)
    if [[ $ARGS == 'git co'* ]]; then
        _fzf_complete "--reverse --multi" "$@" < <(
            echo $branches
        )
    else
        eval "zle ${fzf_default_completion:-expand-or-complete}"
    fi
}

_fzf_complete_git_post() {
    awk '{print $1}'
}

_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}

# trying it
_fzf_comprun() {
  local command=$1
  shift

  case "$command" in
    cd)           fzf "$@" --preview 'tree -C {} | head -200' ;;
    export|unset) fzf "$@" --preview "eval 'echo \$'{}" ;;
    ssh)          fzf "$@" --preview 'dig {}' ;;
    *)            fzf "$@" ;;
  esac
}


#export FZF_COMPLETION_OPTS='-x'
export FZF_DEFAULT_COMMAND='fd --hidden --follow --exclude ".git"'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude ".git"'

#export FZF_COMPLETION_TRIGGER=''
#bindkey '^T' fzf-completion
#bindkey '^I' $fzf_default_completion
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

#asdf
#. /opt/asdf-vm/asdf.sh

# append completions to fpath
#fpath=(${ASDF_DIR}/completions $fpath)
# initialise completions with ZSH's compinit
autoload -Uz compinit
compinit

# rg is better than ag :)
alias ag="rg"

# tail-f
export TAILF_OTP_CACHE=$HOME/tmp/otp-cache
#. ~/tailf/trunk/env.sh

# workaround for shell prompt issues
#export LUX_FLAGS='--shell_prompt_regexp=SH-PROMPT':
#export TYPE=debug
export TYPE=debug
export ERL_AFLAGS="+pc unicode -kernel shell_history enabled -enable-feature all"
export ERL_COMPILER_OPTIONS=[debug_info]
TAILF_SEARCH_PATH="$HOME/tailf"
alias tfenv='source ~/.bin/.tfenv'
alias tfjump='source ~/.bin/.tfjump'
# set to empty to silence direnv logging
export DIRENV_LOG_FORMAT=""
eval "$(direnv hook zsh)"
