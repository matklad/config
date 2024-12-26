#!/bin/sh

USER=$1
HOST=$2
if [[ $USER == "" || $HOST == "" ]] ; then
	echo "usage: `basename $0` user host command"
	exit 1
fi
shift 2

LOCAL_DIR=`pwd`
REMOTE_DIR=`echo $LOCAL_DIR | sed -e "s/Users/home/"`

ssh $USER@$HOST "cd $REMOTE_DIR && $*"
