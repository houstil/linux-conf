

# a little script to install conf (through creating symlinks) on a new machine

verbose=

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
# $2 : the symlink (default is ~/$1)
linkc ()
{
    echov "Installing $1 ..."
    
    slink=$2
    if [ -z "$slink" ]; then
	slink="$HOME/$1"
	echov "symlink is : $slink"
    fi

    conf="`pwd`/$1"
	
    if [ -h "$slink" ]; then
	cv unlink $slink
    elif [ -f "$slink" ]; then
	cv mv $slink $slink.back
    fi
    cv ln -s $conf $slink
}

# a function to show symlinks to apply for root conf
# advising the user to update or not
# $1 : linux-conf file
# $2 : root file (default is /root/$1)
cproot ()
{
    conf=$1
    root=$2
    if [ -z "$root" ]; then
	root="/root/$1"
	echov "dest is : $root"
    fi

    echo "ln -s `pwd`/$conf $root"
}

# an array of conf file and destinations
conf_array=(\
    .Xmodmap "" \
    .bashrc "" \
    .mrxvtrc "" \
#    .ratpoisonrc "" \
    .screenrc "" \
#    .synergy.conf "" \
    .tcshrc "" \
#    .tmux.conf "" \
    .zshrc "" \
#    emc.sh ~/bin/emc.sh \
#    terminator.conf ~/.config/terminator.config \
#    .gitconfig ~/.gitconfig \
    # emacs-conf/bookmarks ~/.emacs.d/bookmarks \
    # emacs-conf/ergoemacs-layout-us.el ~/.emacs.d/el-get/ergoemacs-keybindings/ergoemacs-layout-us.el \
    # emacs-conf/init.el ~/.emacs.d/init.el \
    # emacs-conf/aliases-conf.el ~/.emacs.d/aliases-conf.el \
    # emacs-conf/apparance-conf.el ~/.emacs.d/apparance-conf.el \
    # emacs-conf/bindings-conf.el ~/.emacs.d/bindings-conf.el \
    # emacs-conf/coding-conf.el ~/.emacs.d/coding-conf.el \
    # emacs-conf/files-conf.el ~/.emacs.d/files-conf.el \
    # emacs-conf/edit-conf.el ~/.emacs.d/edit-conf.el \
    # emacs-conf/connection-conf.el ~/.emacs.d/connection-conf.el \
    # emacs-conf/myterm-conf.el ~/.emacs.d/myterm-conf.el \
    # emacs-conf/orgmode-conf.el ~/.emacs.d/orgmode-conf.el \
    # emacs-conf/userint-conf.el ~/.emacs.d/userint-conf.el \
    # emacs-conf/versiondiff-conf.el ~/.emacs.d/versiondiff-conf.el \
    # emacs-conf/pacmans-conf.el ~/.emacs.d/pacmans-conf.el \
)

for ((i=0; i<${#conf_array[@]};i=i+2))
do
    linkc ${conf_array[$i]} ${conf_array[(($i+1))]}
done

echov "to use root config file, do as root :"
# cproot .bashrc.root /root/.bashrc
cproot .zshrc.root /root/.zshrc
cproot emacs-conf/init.root.el /root/.emacs.d/init.el

