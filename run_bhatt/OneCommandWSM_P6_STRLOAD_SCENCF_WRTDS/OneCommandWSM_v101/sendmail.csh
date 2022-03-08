#!/bin/csh

source ../.config/${user}_CONFIG

set PREFIX   = $argv[1]

set SCENARIO = $argv[2]
set LOGFILE  = $argv[3]

set SUBJECT  = ${PREFIX}--${SCENARIO}

#set EMAIL_FROM  = slurm@chesapeakebay.net
#set EMAIL_TO    = ${user}@chesapeakebay.net

echo $SUBJECT
echo $EMAIL_TO
echo $LOGFILE

#/bin/mail -s $SUBJECT $EMAIL < $LOGFILE

python mail5.py $EMAIL_FROM $EMAIL_TO $SUBJECT $LOGFILE $EMAIL_SERVER
