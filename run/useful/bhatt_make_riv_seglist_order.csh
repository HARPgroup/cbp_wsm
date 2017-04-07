#!/bin/csh

if ( ${#argv} < 1 ) then
     echo "Usage: bhatt_make_riv_seglist_order.csh RIV_SEGLIST"
endif

set RIV_SEGLIST = $argv[1]

# fragment the river-segment-list based on stream-order
set text = "Press the <ENTER> key to continue..."
#source ../fragments/set_tree
cd ../make_seglists

csh divide_rseg_by_stream_order.csh DUMMY $RIV_SEGLIST

cd ../../config/seglists
printf "\nGenerating list of river segments based on order:\n"
printf "(a) ./config/seglists/${RIV_SEGLIST}_order.riv\n"
printf "(b) Folder: ./config/seglists/${RIV_SEGLIST}_order\n"
mkdir ${RIV_SEGLIST}_order
mv ${RIV_SEGLIST}_order_*.riv ${RIV_SEGLIST}_order/
set   orderlist = "set order = ( `ls ${RIV_SEGLIST}_order/` )"
echo $orderlist > ${RIV_SEGLIST}_order.riv

find . -name ${RIV_SEGLIST}_order.riv | xargs perl -pi -e 's/\.riv//g'

printf "Next edit List of River Order... (hint: verify formatting)\n\n$text"; set junk = ($<)
vim ${RIV_SEGLIST}_order.riv
echo; echo 'please wait...'
sleep 2
