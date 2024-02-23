#!/bin/csh

alias MATH 'set \!:1 = `echo "\!:3-$" | bc -l`'

set SCENARIO = $argv[1]
set RSEG     = $argv[2]
set TMPDIR   = $argv[3]

mkdir -p ../../tmp/${user}-scratch/$TMPDIR
cd ../../tmp/${user}-scratch/$TMPDIR

echo "... Running river_special_action.csh"

set TEMPFILE = `uuidgen`
set SPEC_ACT = `uuidgen`

set line = "`grep $RSEG ../../../input/scenario/river/spec_flags/p600/spec_flags.csv`"

set data = `echo $line:q | sed 's/,/ /g'`

if ( $data[2] == 0 ) then
   echo "... No special action defined for $RSEG"
else
   echo "... processing: $RSEG"
   set CALIBSCEN = `grep -A1 CALIBSCEN ../../../config/control/river/${SCENARIO}.con | sed -n '2,2p'`
   echo "... using calibration scenario: $CALIBSCEN"

   cat ../../../tmp/uci/river/${SCENARIO}/${RSEG}.uci > $TEMPFILE
   echo "special action file ../../tmp/${user}-scratch/$TMPDIR/$SPEC_ACT"
   head -n -4 $TEMPFILE > ../../../tmp/uci/river/${SCENARIO}/${RSEG}.uci
   rm $TEMPFILE

   if ( $SCENARIO == $CALIBSCEN ) then
      cat ../../../input/scenario/river/spec_flags/p600/SPECACT_CALIBRATION_${RSEG} >> ../../../tmp/uci/river/${SCENARIO}/${RSEG}.uci
   else
      foreach line ( "`cat ../../../input/scenario/river/spec_flags/p600/SPECACT_SCENARIO_${RSEG}`" )
         #echo "$line"
         echo "$line" | awk -F, '{print substr($0,43,8)}' > $TEMPFILE
         set VAL0 = "`cat $TEMPFILE`"
         #echo "$VAL0"
         if ( "$VAL0" == 'SDPM   6' ) then

            set scenmod = "`grep TSSX ../../../output/river/scenario_compare/${SCENARIO}/${RSEG}.csv`"
            echo $scenmod
            set data = `echo $scenmod:q | sed 's/,/ /g'`

            echo "$line" | awk -F, '{print substr($0,1,60)}' > $TEMPFILE
            set VAL1 = "`cat $TEMPFILE`"

            echo "$line" | awk -F, '{print substr($0,61,10)}' > $TEMPFILE
            set VAL2 = "`cat $TEMPFILE`"

            echo $data[3] $data[9]
            set scenval = `echo $data[3] | awk -F"E" 'BEGIN{OFMT="%10.10f"} {print $1 * (10 ^ $2)}'`
            set calval  = `echo $data[9] | awk -F"E" 'BEGIN{OFMT="%10.10f"} {print $1 * (10 ^ $2)}'`
            MATH VAL2 = ( $scenval / $calval ) * $VAL2

            echo "${VAL1}`printf "%10f" ${VAL2}`" >> $SPEC_ACT

            echo "$line"
            echo "${VAL1}${VAL2}"
         else
            echo "$line" >> $SPEC_ACT
         endif
      end
      cat $SPEC_ACT >> ../../../tmp/uci/river/${SCENARIO}/${RSEG}.uci
   endif

endif

cd ..
rm -r $TMPDIR
