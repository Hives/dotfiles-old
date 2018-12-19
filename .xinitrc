[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

errorlog="${HOME}/.xerrors"
if (cp /dev/null "${errorlog}"); then
  chmod 600 "${errorlog}"
  exec >"${errorlog}" 2>&1
fi


# vvvv xmonad vvvvv
# Start daemons and services

xscreensaver -no-splash &
"${HOME}/.dropbox-dist/dropboxd" &
autokey-gtk &
# nm-applet --sm-disable &
urxvtd -q -f -o
numlockx &

# gnome keyring, necessary for mailspring
export $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh,gpg)
dbus-update-activation-environment --systemd DISPLAY
 
if [ -x /usr/bin/gnome-power-manager ] ; then
   sleep 3
   gnome-power-manager &
fi

$HOME/.scripts/remap-keys-iso &

$HOME/.monitors/laptop-only

# loads dark scheme, including wallpaper
$HOME/.scripts/dark-scheme &

compton &

cd $HOME

exec xmonad
