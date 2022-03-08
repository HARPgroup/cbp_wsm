#!/bin/csh

source ../.config/${user}_CONFIG

set PREFIX   = $argv[1]

set SCENARIO = $argv[2]
set LOGFILE  = $argv[3]

source ../../config/control/script/$SCENARIO.con
if ( ($?EMAIL_TO_WSM) ) then
   set EMAIL_TO = $EMAIL_TO_WSM
endif

set SUBJECT  = ${PREFIX}--${SCENARIO}

#set EMAIL_FROM  = slurm@chesapeakebay.net
#set EMAIL_TO    = ${user}@chesapeakebay.net

echo $SUBJECT
echo $EMAIL_TO
echo $LOGFILE

#/bin/mail -s $SUBJECT $EMAIL < $LOGFILE
#echo 'PYTHON PATH'
#which python
#python mail5.py $EMAIL_FROM $EMAIL_TO $SUBJECT $LOGFILE $EMAIL_SERVER

cat $LOGFILE | mail -r slurm@cloudfish.chesapeakebay.net -s $SUBJECT $EMAIL_TO
