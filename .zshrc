 # -*- mode: Shell-script -*-

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob notify
unsetopt beep
bindkey -e

# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/g178452/.zshrc'
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

##############################
# ERGOEMACS LIKE KEYBINDINGS #
##############################

if [ $TERM != "eterm-color" ]
    then
# navigation keys
    bindkey -e '^[k' down-line-or-history
    bindkey -e '^[i' up-line-or-history
    bindkey -e '^[j' backward-char 
    bindkey -e '^[l' forward-char
    bindkey -e '^[u' emacs-backward-word
    bindkey -e '^[o' emacs-forward-word
    bindkey -e '^[J' beginning-of-line
    bindkey -e '^[L' end-of-line

# kill text keys
    bindkey -e '^[e' backward-delete-word
    bindkey -e '^[r' delete-word
    bindkey -e '^[d' backward-delete-char
    bindkey -e '^[f' delete-char
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
	PROMPT="%B%F{cyan}(%F{grey}%M%F{cyan})-(%F{grey}j:%j%F{cyan})-(%F{grey}%~%F{cyan})
(%F{grey}!%!%F{cyan})-%# %f%b"

    else
	PROMPT="%B%F{cyan}(%F{white}%M%F{cyan})-(%F{white}j:%j%F{cyan})-(%F{white}%~%F{cyan})
(%F{white}!%!%F{cyan})-%# %f%b"
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

alias rb='source ~/.zshrc'


#################
# ZSH FUNCTIONS #
#################

function en {
    # h1=$(history|tail -n 1| grep -o "^[^;]*;")
    # if [ -z "$h1" ]
    # then
    #     h1=$(history|tail -n 2|head -n 1)
    # fi
    # notify-send "Terminated :" "$h1"
    echo $@
}


#################################
# LOAD B SHELLS COMPATIBLE CONF #
#################################

source ~/linux-conf/.Xshrc.shared
