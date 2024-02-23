#!/bin/csh

  if (${#argv} != 6 ) then
    echo 'usage: run_nps_and_ps_to_request.csh RIVER_SCENARIO Request YEAR1 YEAR2'
    echo '   will look for files in '
    echo '   ../../catdir/geo/p532/request/p532_//Request//_lrsegs.csv'
    echo '   ../../catdir/geo/p532/request/p532_//Request//_rsegs.csv'
    echo '   ../../catdir/geo/p532/request/p532_//Request//_lrsegs_PS.csv'

#    echo '   ../../tmp/wdm/RIVER_SCENARIO/stream'
    exit
  endif

  set rscen = $argv[1]
  set Request = $argv[2]  
  set year1 = $argv[3]
  set year2 = $argv[4]
  set CH3D  = $argv[6]

  source ../fragments/set_tree
  set tempdir = $argv[5]
  mkdir -p ../../tmp/${user}-scratch/$tempdir/
  cd ../../tmp/${user}-scratch/$tempdir/
  #mkdir -p ../../tmp/scratch/temp$$/
  #cd ../../tmp/scratch/temp$$/

      #  hotstart flag: zero for no hotstart, 1 for write, 2 for read
  set hotstart = 0   

  if (!(-e $tree/tmp/wdm/river/$rscen)) then
    echo 'could not find ' $tree/tmp/wdm/river/$rscen
    echo ' check input parameters'
    exit
  endif

#  echo 'writing date include file'
#  set codedir = ${tree}'/code/src/wqm_load/p5_and_ps_to_request/'
#  set fnam = ${codedir}'date.inc'
#  echo "       parameter ( year1 = $year1 )" > $fnam
#  echo "       parameter ( year2 = $year2 )" >> $fnam
#  echo 'compiling'

#  cd $codedir
#  ./compile
#  cd $tree/tmp/scratch/temp$$/

  mkdir -p $tree/output/wqm_input/$Request/$rscen

  echo 'running'

  if (-e problem) then
    rm problem
  endif

  echo $rscen $hotstart $Request $year1 $year2 $CH3D | $tree/code/bin/p5_sho_to_request_y1y2_P6_RPA_RIB_PSMETHOD.exe

  if (-e problem) then
    cat problem
    rm problem
    exit
  endif

#  echo $rscen  $hotstart $Request $year1 $year2 | $tree/code/bin/p5_ps_to_request_ch3d.exe

  if (-e problem) then
    cat problem
    rm problem
    exit
  endif

######### self-documentation
  set notefile =  ${tree}/output/wqm_input/$Request/$rscen/AutoNotesSHO_${year1}_${year2}
  if (-e $notefile) then
    rm $notefile
  endif

  echo 'This SHO WQM Input data for ' ${Request} ' was created by' $user ' on ' `date` >> $notefile
  echo ' ' >> $notefile

  echo 'Using the code: code/bin/p5_ps_to_request_y1y2.exe' >> $notefile
  echo ' ' >> $notefile

  echo 'and the data from WSM scenario: ' $rscen  >> $notefile
  echo ' '

  cd ../
  rm -r $tempdir

