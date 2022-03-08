#!/bin/csh

  set scenario = $argv[1]

  source ../../config/control/script/$scenario.con

  source ../fragments/set_tree

  set tempdir = $argv[2]
  mkdir -p ../../tmp/${user}-scratch/$tempdir/
  cd ../../tmp/${user}-scratch/$tempdir/
  #mkdir -p ../../../tmp/scratch/temp$$
  #cd ../../../tmp/scratch/temp$$

############ USER DEFINED VARIABLES, SET THESE BELOW APPROPRIATELY
  set code           = $tree/code/bin/create_septic_wdms_from_scenario_csv
  set year1          = $SEPTIC_YEAR1
  set year2          = $SEPTIC_YEAR2
  set SepticScenario = $SB_SCENARIO
  set DataFile       = septic_${SB_SCENARIO}.csv
############ END OF USER VARIABLES

######## make directory
  mkdir -p $tree/input/scenario/river/septic/$SepticScenario/

####### run the code
  if (-e problem) then
    rm problem
  endif

  echo $year1 $year2 $DataFile $SepticScenario | $code

  if (-e problem) then
    cat problem
    exit
  endif

######### self-documentation
  set notefile =  $tree/input/scenario/river/septic/$SepticScenario/AutoNotes
  if (-e $notefile) then
    rm $notefile
  endif

  echo 'Septic WDMs for scenario: ' ${SepticScenario} ' was create by ' $user ' on ' `date` > $notefile
  echo ' ' >> $notefile

  echo 'Using the code: ' $code >> $notefile
  echo ' ' >> $notefile

  echo 'and the data file: input/unformatted/septic/'${DataFile} >> $notefile
  echo ' ' >> $notefile

  cd ../
  rm -r $tempdir
