# -*- mode: Shell-script -*-

# this a shared configuration file for the bash shell

# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# for ido-style cd :
# source ~/bin/bash-ido.sh

# disable this annoying visible bell :
set bell-style none

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize


# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
    screen)      color_prompt=yes;;
    rxvt)        color_prompt=yes;;
    xterm)       color_prompt=yes;;
    eterm-color) color_prompt=yes;;
    # in case of ssh, we don't how the display support colors
    linux)       color_prompt=no;;
    *)           color_prompt=no;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1="\[\e[0;32m\](\[\e[0;37m\]\h\[\e[0;32m\])-(\[\e[0;37m\]j:\j\[\e[0;32m\])-(\[\e[0;37m\]\w\[\e[0;32m\])\n(\[\e[0;37m\]!\!\[\e[0;32m\])-\$ \[\e[0m\]"
    if [ "$TERM" = "eterm-color" ]; then
    PS1="\e[0;32m(\e[0m\h\e[0;32m)-(\e[0mj:\j\e[0;32m)-(\e[0m\w\e[0;32m)\n(\e[0m!\!\e[0;32m)-\$ \e[0m"
    fi
else
    PS1="(\h)-(j:\j)-(\w)\n(!\!)-\$ "
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).+
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi


##############################
# ERGOEMACS LIKE KEYBINDINGS #
##############################

set -o emacs

if [ "$TERM" != "eterm-color" ]
    then
# navigation keys
    bind -m emacs '"\eu": next-history'
    bind -m emacs '"\ee": previous-history'
    bind -m emacs '"\en": backward-char'
    bind -m emacs '"\ei": forward-char'
    bind -m emacs '"\el": backward-word'
    bind -m emacs '"\ey": forward-word'
    bind -m emacs '"\eN": beginning-of-line'
    bind -m emacs '"\eI": end-of-line'

# kill text keys
    bind -m emacs '"\ew": backward-kill-word'
    bind -m emacs '"\ep": kill-word'
    bind -m emacs '"\es": backward-delete-char'
    bind -m emacs '"\et": delete-char'
    bind -m emacs '"\ed": kill-line'

# isearch keys
    bind -m emacs '"\ej": forward-search-history'
    bind -m emacs '"\eJ": reverse-search-history'
fi

################
# BASH ALIASES #
################

#directory management
alias i='pushd . > /dev/null ; pushd -n +1;dirs -v | grep -v "^ 0" '
alias d='dirs -v | grep -v "^ 0"'

alias rb='source ~/.bashrc'
alias eb='~/bin/emc.sh ~/.bashrc'

##################
# BASH FUNCTIONS #
##################

function en {
    h1=$(history 1 | grep -o "^[^;]*;")
    if [ -z "$h1" ]
    then
        h1=$(history 2|head -n 1)
    fi
    notify-send "Terminated :" "$h1"
}

#################################
# LOAD B SHELLS COMPATIBLE CONF #
#################################

source ~/linux-conf/.Xshrc.shared

