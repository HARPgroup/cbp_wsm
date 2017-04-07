#!/bin/csh

echo 'Please enter your   (to)  email address: '
echo "Hit ENTER if ${user}@chesapeakebay.net"
set  input    = $<
if ($input == "") then
    set  TO_EMAIL = ${user}@chesapeakebay.net
else
    set  TO_EMAIL = $input
endif

echo 'Please enter slrum (from) email address: '
echo 'Hit ENTER if slurm@chesapeakebay.net'
set  input    = $<
if ($input == "") then
    set  FROM_EMAIL = slurm@chesapeakebay.net
else
    set  FROM_EMAIL = $input
endif

echo 'Please enter email server address (or IP): '
echo 'Hit ENTER if 127.0.0.1'
set  input    = $<
if ($input == "") then
    set  EMAIL_SERVER = '127.0.0.1'
else
    set  EMAIL_SERVER = $input
endif

mkdir -p .config
echo ""                                 >  .config/${user}_CONFIG
echo "set EMAIL_TO     = $TO_EMAIL"     >> .config/${user}_CONFIG
echo "set EMAIL_FROM   = $FROM_EMAIL"   >> .config/${user}_CONFIG
echo ""                                 >> .config/${user}_CONFIG
echo "set EMAIL_SERVER = $EMAIL_SERVER" >> .config/${user}_CONFIG
