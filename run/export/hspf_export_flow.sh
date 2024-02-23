#!/bin/csh
# and if that file exists, we run it, otherwise, skip it
wdm=$1

cp /usr/local/lib/hspf/message.wdm ./
if ( $?START_YEAR ) then
  echo "echo $wdm,$START_YEAR,$END_YEAR,111 | wdm2text"
  echo "$wdm,$START_YEAR,$END_YEAR,111" | wdm2text
endif
rm message.wdm 
