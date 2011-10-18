#!/bin/zsh

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
EOF
}

########################
# Variables definition #
########################

EMACS_BIN="emacs"

# display a message if verbose is turned on
function vp() [ $VERBOSE ] && echo $*

# execute the command and display it if verbose is on
function dc()
{
    [ $VERBOSE ] && echo $*
    $*
}

function lisp_kill()
{
    vp "lisp killing"
    vp "now displaying a frame"
    is_frame || create_frame
    vp "selecting it"
    wmctrl -xa $EMACS_BIN
    emacsclient -e "(save-buffers-kill-emacs)"
    sleep 1
}

function get_pid(){
    local pid=`ps x | grep "[e]macs --daemon" | awk '{print $1}'`
    if [ -n "$pid" ]; then
        return $pid
    else
        return 0
    fi
}

function brutal_kill()
{
  kill -9 $1
  sleep 1
}

function start_server()
{
    dc $EMACS_BIN --daemon
}

function is_frame()
{
    vp "is_frame called"
    local WID=`wmctrl -l | grep -o "emacs@rmm"`
    if [ -n "$WID" ] ;then
        vp "frame found"
        return 0
    else
        vp "frame not found"
        return 1
    fi
}

function create_frame() dc emacsclient -n -c

function open_file()
{
    if [ ! -f "$1" ]; then
        wmctrl -ia $wid
        echo "$1 does not exist, create it ? [n/Y]"
        read creation
        if [ "$creation" = "n" ]; then return 0; fi
    fi

    if [ -n "$2" ]; then
        dc emacsclient -n -e "(find-file-at-line \"$1\" $2)"
    else
        dc emacsclient -n $1
    fi
    oo=1
}

function main()
{
    while getopts "hvrRkKn" OPTION
    do
        case $OPTION in
	    h)
	        usage
	        exit 0
	        ;;
	    v)
	        VERBOSE=1
                vp VERBOSE
	        ;;
            k)
                K=1
                SK=1
                vp K SK
                ;;
	    r)
                SK=1
                vp SK
	        ;;
            K)
                K=1
	        BK=1
                vp K BK
                ;;
	    R)
	        BK=1
                vp BK
	        ;;
	    ?)
	        usage
	        exit
	        ;;
        esac
    done

# to erase options from arg list
    shift $(($OPTIND - 1))

# save caller winid
    local wid=`current-winid`

    [ $BK ] && (get_pid || brutal_kill $?)

    [ $SK ] && (get_pid || lisp_kill)

    [ $K ] && return 0
    
    vp "test if server exist"
    get_pid && dc start_server

    is_frame || create_frame

    [ $* ] && for file in $*; do
        local line=`echo $file |grep -o ":[0-9]*"|tr -d ':'`
        [ $line ] && vp "line found : $line"
        file=`echo $file | sed 's/:[0-9]*$//g'`
        [ $file ] && vp "file renamed : $file"
        open_file $file $line
    done

    ([ ! $* ] || [ $oo ]) && wmctrl -xa $EMACS_BIN

}

# call main function
main $*

