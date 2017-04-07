#!/bin/csh

  if (${#argv} != 4 ) then
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

  source ../fragments/abs_tree
  mkdir -p ../../tmp/scratch/temp$$/
  cd ../../tmp/scratch/temp$$/

      #  hotstart flag: zero for no hotstart, 1 for write, 2 for read
  set hotstart = 1   

  if (!(-e $tree/tmp/wdm/river/$rscen)) then
    echo 'could not find ' $tree/wdm/$rscen
    echo ' check input parameters'
    exit
  endif

  echo 'writing date include file'
  set codedir = ${tree}'/code/src/wqm_load/p5_and_ps_to_request/'
  set fnam = ${codedir}'date.inc'
  echo "       parameter ( year1 = $year1 )" > $fnam
  echo "       parameter ( year2 = $year2 )" >> $fnam
  echo 'compiling'

  cd $codedir
  compile
  cd $tree/tmp/scratch/temp$$/

  mkdir -p $tree/output/wqm_input/$Request/

  echo 'running'

  if (-e problem) then
    rm problem
  endif

  echo $rscen $hotstart $Request | $tree/code/bin/p5_nps_to_request.exe

  if (-e problem) then
    cat problem
    rm problem
    exit
  endif

  echo $rscen  $hotstart $Request | $tree/code/bin/p5_ps_to_request.exe

  if (-e problem) then
    cat problem
    rm problem
    exit
  endif

######### self-documentation
  set notefile =  $tree/output/wqm_input/$Request/AutoNotes
  if (-e $notefile) then
    rm $notefile
  endif
  echo 'This dataset,' ${Request} > $notefile
  echo ' was created by' $user >> $notefile
  echo ' ' >> $notefile
  echo ' on' >> $notefile
  date >> $notefile
  echo ' ' >> $notefile
  echo 'Using the code' >> $notefile
  echo $codedir >> $notefile
  echo ' ' >> $notefile
  echo 'and the data from scenario' >> $notefile
  echo $rscen >> $notefile

  cd ../
  rm -r temp$$

