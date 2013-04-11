#!/bin/bash

# Script to backup important config files from System

HOME='/home/eye/'
#NDIR='/media/roach/backup/'
H=`hostname`
CL="/home/eye/.bak_${H}"
FILESYS='/usr/share/slim/themes/archlinux-me:/usr/share/slim/themes/archlinux-simple:/etc/fstab:/etc/dhcpcd.conf:/etc/pacman.d/mirrorlist:/etc/pacman.conf:/etc/resolv.conf:/etc/locale.conf:/etc/hostname:/etc/timezone:/etc/conf.d/network:/etc/vconsole.conf:/etc/slim.conf:/etc/ntp.conf:/etc/makepkg.conf:/etc/pacman.d/gnupg/gpg.conf:/etc/clamav/clamd.conf:/etc/clamav/freshclam.conf:/etc/abs.conf:/etc/systemd'
FILEHOME='.local/share/applications:.bash*:.config:.firefox:.conky*:.cups:.dmrc:.face:.fehbg:.fonts:.gtk*:.hplip:.htoprc:.mpd*:.mplayer:.ncmpcpp:.pondus:.purple:.sane:.vim*:.xinitrc:.xscreensaver:.themes:.icons:.pulse/client.conf:.zshrc:.zshenv:.omzsh:.zshrc.orig:.functs:.muttrc:.pentadactyl:.pentadactylrc:.vimperator:.vimperatorrc:.Xresources:.xmobarrc:.xmonad:.xmobar'
DEST='/tmp/confbak'
DATE=`date +%F`
FL="/tmp/conf-${DATE}_${H}.tar.lzo"

mkdir /tmp/confbak
for FILE in `echo $FILESYS | sed 's/\:/\n/g'`; do cp -av $FILE $DEST; done
for FILE in `echo $FILEHOME | sed 's/\:/\n/g'`; do cp -av $HOME$FILE $DEST; done

comm -23 <(/usr/bin/pacman -Qeq|sort) <(pacman -Qmq|sort) > $DEST/pkglist


tar -cjpf - $DEST | lzop -9 -o $FL
#chown eye:users $FL
#chmod og-r $FL
rm -rf $DEST

## cleanup old tars

#find /media/backup/conf -mtime +50 -exec rm -rf {} \;

## get filename in cloud folder and replace it
CLFL=$(ls $CL/conf*${H}*bak)
NFL=`echo $FL | sed 's/.*\/\(.*$\)/\1/'`
G="gpg --default-recipient-self"
[[ -f $CLFL ]] || exit 0
if [[ $(stat -c "%Y" $CLFL) -lt $(stat -c "%Y" $FL) ]];then
	rm $CLFL;
	$G -o $CL/${NFL:0:-8}.bak -e $FL;
fi
# finally, copy it to nas
#cp $FL $NDIR
rm $FL
