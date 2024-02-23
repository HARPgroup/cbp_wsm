#!/bin/csh

# Usage: sbatch AEIOU_process.csh AEIOU_N20180213 1X

alias MATH 'set \!:1 = `echo "\!:3-$" | bc -l`'


set BASESCEN = $argv[1]
set AEIOU_ID = $argv[2]

set SCENARIO = AEIOU_$AEIOU_ID

set PARAMS = ( NH3X NO23 RORN BODN ORGN TOTN PO4X PIPX RORP BODP ORGP TOTP )
set line = `grep "^$AEIOU_ID," AEIOU_scenarios`
set data = `echo $line:q | sed 's/,/ /g'`
set RSEG = $data[3]
set RVAR = $data[2]
set LOAD = $data[4]
echo "... performing $RVAR + $LOAD at $RSEG"

goto STEP0

echo "STARTING at `date`"
## 0. SETUP
STEP0:
   #mkdir -vp ../../tmp/wdm/river/$SCENARIO/eos
   mkdir -vp ../../tmp/wdm/river/$SCENARIO
   mkdir -vp ../../tmp/wdm/river/$SCENARIO/stream
   mkdir -vp ../../tmp/uci/river/$SCENARIO
   mkdir -vp ../../output/river/annual/$SCENARIO
   mkdir -vp ../../output/river/aveann/$SCENARIO
   mkdir -vp ../../output/river/scenario_compare/$SCENARIO
   mkdir -vp ../../output/pltgen/river/$SCENARIO
   mkdir -vp ../../output/river/res_eff/$SCENARIO
   mkdir -vp ../../output/hspf/river/out/$SCENARIO
   mkdir -vp ../../output/hspf/river/ech/$SCENARIO

   echo "... starting copying data `date`"
   #cp -p ../../tmp/wdm/river/$BASESCEN/eos/* ../../tmp/wdm/river/$SCENARIO/eos/
   ln -s ../../tmp/wdm/river/$BASESCEN/eos ../../tmp/wdm/river/$SCENARIO/eos
   echo "... finished copying eos `date`"
   cp -p ../../tmp/wdm/river/$BASESCEN/stream/* ../../tmp/wdm/river/$SCENARIO/stream/
   echo "... finished copying stream `date`"
   cp -p ../../output/river/annual/$BASESCEN/*  ../../output/river/annual/$SCENARIO/
   echo "... finished copying river annual `date`"
   cp -p ../../output/river/aveann/$BASESCEN/*  ../../output/river/aveann/$SCENARIO/
   echo "... finished copying river aveann `date`"
   ln -s ../../output/etm/$BASESCEN ../../output/etm/$SCENARIO
   cp -vp ../../config/control/river/$BASESCEN.con ../../config/control/river/$SCENARIO.con

   du -sh ../../tmp/wdm/river/$SCENARIO/eos/
   du -sh ../../tmp/wdm/river/$SCENARIO/stream/
   du -sh ../../output/river/annual/$SCENARIO/
   du -sh ../../output/river/aveann/$SCENARIO/


## 1. CALCULATE FACTOR
STEP1:
 if ( $LOAD > 0 ) then
   set line = "`grep $RVAR ../../output/river/aveann/$BASESCEN/${RSEG}_1985_2014.ave`"
   echo "$line"
   set AVGANNSTR = `echo "$line" | awk '{print substr($0,16,14)}'`
   echo ">${AVGANNSTR}<"
   set AVGANNVAL = `echo $AVGANNSTR | awk -F"E" 'BEGIN{OFMT="%10.10f"} {print $1 * (10 ^ $2)}'`
   MATH FACTOR = 1 + ( $LOAD / $AVGANNVAL )
   echo $FACTOR
 endif


## 2. APPLY FACTOR
STEP2:
  if ( $LOAD > 0 ) then
   set MYPWD = `pwd`
   set TMPDIR = `uuidgen`
   mkdir ../../tmp/gbhatt-scratch/$TMPDIR
   cd    ../../tmp/gbhatt-scratch/$TMPDIR
   cp -vp ../../../tmp/wdm/river/$SCENARIO/stream/$RSEG.wdm .
   echo $SCENARIO $RSEG $RVAR $FACTOR | ../../../code/bin/AdjustLoads.exe
   if ( -e problem ) then
      cat problem
      exit
   endif
   cp -vp $RSEG.wdm ../../../tmp/wdm/river/$SCENARIO/stream/$RSEG.wdm
   cd $MYPWD
   rm -r ../../tmp/gbhatt-scratch/$TMPDIR
  endif
   

## 3. RUN (a) DOWNSTREAM SEGMENT & (b) ANNUAL LOAD ALONG WITH DOWNSTREAM SEGMENTS
STEP3:
  if ( $LOAD > 0 ) then
   csh bhatt_run_postproc_river_aveann_oneseg.csh $SCENARIO $RSEG 1985 2014 temp$SCENARIO
   csh bhatt_run_postproc_river_annual_oneseg.csh $SCENARIO $RSEG 1985 2014 temp$SCENARIO
   set DRSEG = $RSEG
   set DRID  = `echo "$DRSEG" | awk '{print substr($0,10,4)}'`
   set DRSEG = `grep _${DRID}_ ../../config/catalog/geo/p600/rivers.csv`
   while ( ${#DRSEG} > 0 )
      echo "... processing $DRSEG"
      csh bhatt_run_river_oneseg.csh $SCENARIO $DRSEG temp$SCENARIO
      csh bhatt_run_postproc_river_aveann_oneseg.csh $SCENARIO $DRSEG 1985 2014 temp$SCENARIO
      csh bhatt_run_postproc_river_annual_oneseg.csh $SCENARIO $DRSEG 1985 2014 temp$SCENARIO
      #set IRID = `echo "$DRSEG" | awk '{print substr($0,5,4)}'`
      set DRID  = `echo "$DRSEG" | awk '{print substr($0,10,4)}'`
      set DRSEG = `grep _${DRID}_ ../../config/catalog/geo/p600/rivers.csv`
   end
  endif


## 4. ARCHIVE ANNUAL OUTPUTS
STEP4:
   source ../../config/seglists/watershed.riv
   #set PARAMS = ( NH3X NO23 RORN BODN ORGN TOTN PO4X PIPX RORP BODP ORGP TOTP )
   #set line = `grep $AEIOU_ID AEIOU_scenarios`
   #echo "$line"
   #set data = `echo $line:q | sed 's/,/ /g'`
   #echo "$data"
   echo "${RVAR} $RSEG ${LOAD}"
   set ANNUAL = /modeling/gb604/output/river/annual/20190212_AEIOU/ANNUAL_${RSEG}_${RVAR}_${LOAD}.csv
   set AVGANN = /modeling/gb604/output/river/annual/20190212_AEIOU/AVGANN_${RSEG}_${RVAR}_${LOAD}.csv
   if ( -e $ANNUAL ) rm $ANNUAL
   if ( -e $AVGANN ) rm $AVGANN
   foreach PARAM ( $PARAMS )
      foreach segment ( $segments )
         set RID = `echo $segment | awk '{print substr($0,10,4)}'`
         if ( $RID != "0000") then
            foreach line ("`cat ../../output/river/annual/$SCENARIO/${segment}_${PARAM}_year.prn`")
               echo "$segment ${RVAR} $line" >> $ANNUAL
            end
            echo "$segment ${RVAR} `grep $PARAM ../../output/river/aveann/$SCENARIO/${segment}_1985_2014.ave`" >> $AVGANN
         endif
      end
   end

exit
## 5. CLEAN
STEP5:
   #rm -r ../../tmp/wdm/river/$SCENARIO/eos
   rm    ../../tmp/wdm/river/$SCENARIO/eos
   #rm -r ../../tmp/wdm/river/$SCENARIO/stream
   rm -r ../../tmp/wdm/river/$SCENARIO
   rm -r ../../tmp/uci/river/$SCENARIO
   rm -r ../../output/river/annual/$SCENARIO
   rm -r ../../output/river/aveann/$SCENARIO
   rm -r ../../output/river/scenario_compare/$SCENARIO
   rm -r ../../output/pltgen/river/$SCENARIO
   rm -r ../../output/river/res_eff/$SCENARIO
   rm -r ../../output/hspf/river/out/$SCENARIO
   rm -r ../../output/hspf/river/ech/$SCENARIO

   rm ../../output/etm/$SCENARIO


   echo "FINISHING at `date`"
