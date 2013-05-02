# btsync starter/stopper

PIDFILE="/home/eye/.btsync/sync.pid"
case "$1" in
stop)
        echo "stopping BitTorrent Sync..."
        PID=$(cat $PIDFILE)
        kill $PID 
        ;;

start)
        echo "starting BitTorrent Sync..."
        /usr/share/btsync/btsync --config /home/eye/.config/btsync.json
        ;;

restart)
        $0 stop
        sleep 2
        $0 start
        ;;

esac
