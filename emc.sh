#!/bin/bash

# my emacs launcher

usage()
{
cat << EOF
usage: $0 options

This script launch emacs server or open on new emacsclient connection on it

emc.sh [OPTIONS] FILE1 FILE2 ...

OPTIONS:
   -h      Show this message
   -v      Verbose
   -r      Restart the emacs server, saving files before
   -R      Restart the emacs server, brutally killing it
   -k      Kill the emacs server, saving files before
   -K      Kill the emacs server, brutally killing it
   -n      Create a new frame
EOF
}

while getopts "hvrRkKn" OPTION
do
    case $OPTION in
	h)
	    usage
	    exit 1
	    ;;
	v)
	    VERBOSE=1
	    ;;
	r)
	    RESTART=1
	    ;;
	R)
	    RESTART=1
	    BRUTAL=1
	    ;;
	k)
	    KILL=1
	    ;;
	K)
	    KILL=1
	    BRUTAL=1
	    ;;
	n)
	    NFRAME=1
	    ;;
	?)
	    usage
	    exit
	    ;;
    esac
done

# to erase options from arg list
shift $(($OPTIND - 1))

# kill the server if necessary
EMACS_PID=$(ps x | grep "[e]macs23 --daemon" | grep -o "^ *[0-9]*")

if [ -n "$EMACS_PID" ] && [ -n "$KILL" -o "$RESTART" -o -n "$BKILL" ]; then
    if [ -n "$BRUTAL" ]
    then
	[ $VERBOSE ] && echo "Brutal killing emacs server ..."
	kill -9 $EMACS_PID 
    else
	[ $VERBOSE ] && echo "Killing emacs server ..."
	
        # create a frame for the prompt if necessary
	WID=$(wmctrl -l | grep -o "emacs23@rmm")
	if [ -z "$WID" ] ; then
        # we create a new frame
	    [ $VERBOSE ] && echo "Launch emacsclient in new frame ..."
	    emacsclient --no-wait --create-frame
	else
	    wmctrl -xa emacs
	fi
	emacsclient --eval "(save-buffers-kill-emacs)"
    fi
fi

# if just kill we are done
if [ -n "$KILL" ]; then
    exit 1
fi

# start the server if necessary
EMACS_PID=$(ps x | grep "[e]macs23 --daemon")
if [ -z "$EMACS_PID" ]; then
    [ $VERBOSE ] && echo "Starting emacs server ..."
    emacs23 --daemon
fi

# launch the client and read the files
WID=$(wmctrl -l | grep -o "emacs23@rmm")

if [ -n "$WID" ] && [ -z "$NFRAME" ]; then
    # we don't create a new frame
    [ $VERBOSE ] && echo "Launch emacsclient ..."
    emacsclient --no-wait $*
    wmctrl -xa emacs
else
    # we create a new frame
    [ $VERBOSE ] && echo "Launch emacsclient in new frame ..."
    emacsclient --no-wait --create-frame $*
fi
