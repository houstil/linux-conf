#!/bin/bash

# a little script to install conf (through creating symlinks) on a new machine

verbose=1

# a function to print data if verbose is on
echov ()
{
    (($verbose)) && echo $*
}

# a function to exec a command and print it if verbose is on
# $1 : text to print
cv ()
{
    echov $*
    eval $*
}

# a function to replace any existing file by a linkg, saving it to $file.back
# $1 : the config file to be linked 
# $2 : the symlink (default is ~/<config file name>)
linkc ()
{
    slink=$2
    if [ -z "$slink" ]; then
	slink="$HOME/$1"
	echo "symlink is : $slink"
    fi

    conf="`pwd`/$1"
	
    if [ -h "$slink" ]; then
	cv unlink $slink
    elif [ -f "$slink" ]; then
	cv mv $slink $slink.back
    fi
    cv ln -s $conf $slink
}

for f in .Xmodmap .bashrc .mrxvtrc .screenrc .tcshrc .tmux.conf .zshrc
do
    echov "Installing $f ..."
    linkc $f
done    