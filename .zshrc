 # -*- mode: Shell-script -*-

# Just to be sure
SHELL=/bin/zsh

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
ZSHDOTDIR=$HOME/.zsh.d
setopt appendhistory autocd extendedglob notify
unsetopt beep
bindkey -e

# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '~/.zshrc'
# End of lines added by compinstall

autoload -U select-word-style
select-word-style bash

# to enable case insensitive completion
setopt extendedglob
unsetopt CASE_GLOB
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'


## Completions
autoload -U compinit
compinit #-C
## completions ####
autoload -U zstyle+
## General completion technique
## complete as much u can ..
zstyle ':completion:*' completer _complete _list _oldlist _expand _ignored _match _correct _approximate _prefix
zstyle ':completion:*' menu select=1

## complete less
#zstyle ':completion:*' completer _expand _complete _list _ignored _approximate
## complete minimal
#zstyle ':completion:*' completer _complete _ignored

## allow one error
zstyle ':completion:*:approximate:*' max-errors 1 numeric
## allow one error for every three characters typed in approximate completer
# zstyle -e ':completion:*:approximate:*' max-errors \
# 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'

# Emacs ansi-term directory tracking
# track directory, username, and cwd for remote logons
case "$TERM" in
    xterm)
	chpwd   () { print -Pn "\e]2;%n@%m:%~\a" }
	precmd  () { print -Pn "\e]2;%n@%m:%~\a" }
        preexec () { print -Pn "\e]2;%n@%m:%~\a" }
	;;
    eterm-color)
	chpwd   () { echo -e "\032/$(pwd)" }
	precmd  () { echo -e "\032/$(pwd)" }
	;;

esac

# to reverse the incremental search in history direction
# bindkey -e '^[s' history-incremental-search-forward

# to ensure that home and end keys work properly :
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line
bindkey "^[[7~" beginning-of-line
bindkey "^[[8~" end-of-line
bindkey "^[[H" beginning-of-line
bindkey "^[[F" end-of-line


##############################
# ERGOEMACS LIKE KEYBINDINGS #
##############################

ERGO_ZSH="1"
if [ $ERGO_ZSH ]
then
    if [ $TERM != "eterm-color" ]
    then
# navigation keys
	bindkey -e '^[e' down-line-or-history
	bindkey -e '^[u' up-line-or-history
	bindkey -e '^[n' backward-char 
	bindkey -e '^[i' forward-char
	bindkey -e '^[l' emacs-backward-word
	bindkey -e '^[y' emacs-forward-word
	bindkey -e '^[N' beginning-of-line
	bindkey -e '^[I' end-of-line

# kill text keys
	bindkey -e '^[f' backward-delete-word
	bindkey -e '^[p' delete-word
	bindkey -e '^[s' backward-delete-char
	bindkey -e '^[t' delete-char
    fi
fi

#####################
# PROMPT DEFINITION #
#####################

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
    screen)      color_prompt=yes;;
    rxvt)        color_prompt=yes;;
    xterm)       color_prompt=yes;;
    eterm-color) color_prompt=yes;;
    # in case of ssh, we don't how the display support colors
    linux)       color_prompt=no;;
esac

if [ "$color_prompt" = yes ]
then
    if [ "$TERM" = "eterm-color" ]; then
	# PROMPT="\e[32m(\e[0m%M\e[32m)-(\e[0mj:%j\e[32m)-(\e[0m%~\e[32m)\n(\e[0m!%!\e[32m)-%# \e[0m"
	PROMPT="%F{cyan}(%F{grey}%M%F{cyan})-(%F{grey}j:%j%F{cyan})-(%F{grey}%~%F{cyan})
(%F{grey}!%!%F{cyan})-%# %f"

    else
	PROMPT="%F{cyan}(%F{white}%M%F{cyan})-(%F{white}j:%j%F{cyan})-(%F{white}%~%F{cyan})
(%F{white}!%!%F{cyan})-%# %f"
    fi

else
    PROMPT="(%M)-(j:%j)-(%~)
(!%!)-%# "
fi

#to get a shy pushd
setopt PUSHD_SILENT

###############
# ZSH ALIASES #
###############

#directory management
alias d='dirs -v | grep -v "^0"'
alias i='pushd .;pushd +1'
alias j=bj

alias rb='source ~/.zshrc'
alias eb='~/bin/emc.sh ~/.zshrc'


#################
# ZSH FUNCTIONS #
#################

function en {
    notify-send "Terminated :" "`history | tail -1 | sed 's/^ *[0-9]* *\(.*\)$/\1/g'`" -t 3000
}

#################
# ZSH BOOKMARKS #
#################
# credit to benboeckel
_bookmarks_file=$ZSHDOTDIR/bookmarks

# Functions
be () {
    $EDITOR ${_bookmarks_file}
}

ba () {
    if [ -z "$1" ]; then
        echo "Error: Argument required"
        return 1
    fi
    echo "$1    $PWD" >> ${_bookmarks_file}
    sort ${_bookmarks_file} -o ${_bookmarks_file}
}

bj () {
    local nd
    nd=`sed -ne "/^$1/s/^$1    //p" ${_bookmarks_file}`
    if [[ -n $nd ]]; then
        cd $nd
    fi
}

br () {
    sed -i "/^$1    .*$/d" ${_bookmarks_file}
}

bm () {
    br $1
    ba $1
}

bl () {
    cat ${_bookmarks_file}|awk '{printf "%15s\t%s\n",$1, $2}'
}

_bj () {
    local -a _bookmarks
    _bookmarks=( $(sed -e 's/   .*$//' ${_bookmarks_file}) )
    _values 'bookmarks' \
        $_bookmarks
}
compdef _bj bj
compdef _bj br
compdef _bj bm


#################################
# LOAD B SHELLS COMPATIBLE CONF #
#################################

source ~/linux-conf/.Xshrc.shared
