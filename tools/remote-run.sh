#!/bin/sh

USER=$1
HOST=$2
if [[ $USER == "" || $HOST == "" ]] ; then
	echo "usage: `basename $0` user host command"
	exit 1
fi
shift 2

$(dirname "$0")/remote-sync.sh $USER $HOST

LOCAL_DIR=`pwd`
REMOTE_DIR=`echo $LOCAL_DIR | sed -e "s/Users/home/"`

ssh -t $USER@$HOST "cd $REMOTE_DIR && $*"
