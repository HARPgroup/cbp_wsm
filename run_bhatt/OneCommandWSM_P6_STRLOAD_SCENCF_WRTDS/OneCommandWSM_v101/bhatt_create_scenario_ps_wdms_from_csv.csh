#!/bin/csh

  set scenario = $argv[1]

  source ../../config/control/script/$scenario.con

  source ../fragments/set_tree

  set tempdir = $argv[2]
  mkdir -p ../../tmp/${user}-scratch/$tempdir/
  cd ../../tmp/${user}-scratch/$tempdir/
  #mkdir -p ../../../tmp/scratch/temp$$/
  #cd ../../../tmp/scratch/temp$$/

############ USER DEFINED VARIABLES, SET THESE BELOW APPROPIRATELY
  set code        = $tree/code/bin/create_scenario_ps_wdms_from_csv.exe
  set dataversion = $PS_DATAVERSION
  set WWTPfnam    = $WWTP
  set INDfnam     = $IND
  set CSOfnam     = $CSO
  set CSOaveyear1 = $CSO_AVG_YEAR1
  set CSOaveyear2 = $CSO_AVG_YEAR2
  set psscen      = $PS_SCENARIO
############ END OF USER VARIABLES

######## make directories
  mkdir -p $tree/input/scenario/river/ps/$psscen/
  mkdir -p $tree/input/scenario/river/ps/$psscen/bay_models/

####### run the point source code
  if (-e problem) then
    rm problem
  endif

  echo $psscen $dataversion $WWTPfnam $INDfnam $CSOfnam $CSOaveyear1 $CSOaveyear2 | $code

  if (-e problem) then
    cat problem
    exit
  endif

######### self-documentation
  set notefile =  $tree/input/scenario/river/ps/$psscen/AutoNotes
  if (-e $notefile) then
    rm $notefile
  endif

  echo 'PS WDMs for scenario: ' ${psscen} ' was created by ' $user ' on ' `date` > $notefile
  echo ' ' >> $notefile

  echo 'Using the code ' $code >> $notefile
  echo ' ' >> $notefile

  echo 'and the PS data files:' >> $notefile
  echo '../../../unformatted/point_source/'${dataversion}/${WWTPfnam} >> $notefile
  echo ' ' >> $notefile
  echo '../../../unformatted/point_source/'${dataversion}/${INDfnam} >> $notefile
  echo ' ' >> $notefile
  echo '../../../unformatted/point_source/'${dataversion}/${CSOfnam} >> $notefile  
#  echo ' and ' >> $notefile
#  echo '../../../unformatted/point_source/'${dataversion}/${CSOfacfnam} >> $notefile  

########## get rid of temp directory
  cd ../
  rm -r $tempdir
