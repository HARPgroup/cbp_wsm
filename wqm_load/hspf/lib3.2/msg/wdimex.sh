#!/bin/sh
#  msg/wdimex.sh -- make generic message.wdm for libraries
#
# History:  95/12/05 kmflynn  clean up for tape 3
#
# Usage: wdimex.sh [file_name [parm_version]] [| tee output_file]
#
# Where: file_name = the name of the message file to build
#                    message - to generate generic message file (default)
#                    hspf - to generate hspf message file
#        parm_version is the version of the parm__.seq file
#                    dg - data general using prior-gks (default)
#                    ux - unix using Xgks
#                    pc - using interactor
#
# Examples:  wdimex.sh message dg
#
# Note:  the ___.in files are left in the direcory for the pc make, kmf

#*******************************************************************
#***** You should not need to modify anything below this line. *****
#*******************************************************************

Optn=${1-'message'}
     if [ ! $Optn ] ; then Optn=message ; fi
Vers=${2-'dg'}
     if [ ! $Vers ] ; then Vers=dg ; fi


binDir=../bin
LibDat=../lib_data

# system dependent sequential file
  Parm=aide/parm$Vers

# sequential files for generic message.wdm
  NameM=message
  SeqM='aide/message waide/awfeb waide/prwfil waide/atinfo'


# sequential files for generic hspfmsg.wdm
  NameH=hspfmsg
  SeqG='aide/message
        waide/awfeb   waide/tslist  waide/agplot
        awstat/tscmpr awstat/a193   awstat/profdr
        ann/pgener    ann/qtprnt'
  SeqH='hiouci   hprbut   hruntspt hrinoput himpqua  hspfitab hrch
        hdatut   hringeut htsinsi  hringen  hspf     hspfec   hpersno
        hperpho  hpernit  hperpes  hrchnut  hperwat  hrchaci  hrchgqu
        hrchhyd  hrchphc  hrchsed  hrchhtr  hrchplk  hwdmut   hdssut
        hdssx    hutop    hutopinp hrunut   hrinseq  hrinwdm  hrindss
        hruntsgw himp     himpwat  hperagut hruntsgq hperqua  hper
        hruntsgp hruntsgt hruntspw hutdura  hruntsut hruntsgd hruntspd
        hrints   hrintss  htssut   specact  perlndts implndts rchrests
        copyts   pltgents displyts duranlts generts  mutsints perlnd
        implnd   rchres   copy     pltgen   disply   gener    duranl
        mutsin'                                                        
  SeqN='sgtabl   agmap    prwplt   ucimod   ucirea   wsgsim   wsgutl
        dspeci   wsgsys   tsplot   durani   tsfreq   sturbn'

  echo
  echo ' parm__.seq version:' parm$Vers.seq
  echo ' building wdm files:' $Optn
  echo
  if [ $Optn = 'message'  -o  $Optn = 'both' ]
    then
#   build the generic message file
    echo
    echo ' building the generic message file' $NameM'.wdm'
    echo
  
#   remove any old files
    if [ -f error.fil ]          ; then rm error.fil          ; fi
    if [ -f $NameM.in ]          ; then rm $NameM.in          ; fi
    if [ -f $NameM.wdm ]         ; then rm $NameM.wdm         ; fi
    if [ -f $LibDat/$NameM.wdm ] ; then rm $LibDat/$NameM.wdm ; fi
  
#   build input file
    echo $NameM.wdm > $NameM.in
    echo C >> $NameM.in
    echo Y >> $NameM.in
    echo adwdm/ >> $NameM.in
    for Seq in $SeqM $Parm ; do
       echo I >> $NameM.in
       echo $Seq.seq >> $NameM.in
    done
    echo R >> $NameM.in
  
#   build message file
    $binDir/wdimex < $NameM.in

#   move new message file and clean up files
    mv $NameM.wdm $LibDat/$NameM.wdm

  fi


  if [ $Optn = 'hspf'  -o  $Optn = 'both' ]
    then
#   build the generic hspf message file
    echo
    echo ' building the generic hspf message file' $NameH'.wdm'
    echo

#   remove any old files
    if [ -f error.fil ]          ; then rm error.fil          ; fi
    if [ -f $NameH.in ]          ; then rm $NameH.in          ; fi
    if [ -f $NameH.wdm ]         ; then rm $NameH.wdm         ; fi
    if [ -f $LibDat/$NameH.wdm ] ; then rm $LibDat/$NameH.wdm ; fi
  
#   build input file
    echo $NameH.wdm > $NameH.in
    echo C >> $NameH.in
    echo Y >> $NameH.in
    echo adwdm/ >> $NameH.in
    for Seq in $SeqG $Parm ; do
       echo I >> $NameH.in
       echo $Seq.seq >> $NameH.in
    done
    for Seq in $SeqH ; do
       echo I >> $NameH.in
       echo hspf/$Seq.seq >> $NameH.in
    done
    for Seq in $SeqN ; do
       echo I >> $NameH.in
       echo newaqt/$Seq.seq >> $NameH.in
    done
    echo R >> $NameH.in
  
#   build message file
    $binDir/wdimex < $NameH.in
  
#   move new message file and clean up files
    mv $NameH.wdm $LibDat/$NameH.wdm

  fi

# end of shell
