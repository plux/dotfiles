#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then

    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then

    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# launch emacs
emacs &
# turn off beep
xset b off
# blank screen after 10 minutes
xset s on
xset s 600
# use xsecurelock to lock screen
xss-lock xsecurelock &
# launch terminal
termite &
# launch x compositor
picom --backend glx --mark-ovredir-focused &
# set background to blackx
hsetroot -solid black &
#eval $(ssh-agent) &
# launch network manager
nm-applet &
# launch polybar with delay so workspaces show up
eval "sleep 2; polybar main" &
exec xmonad
