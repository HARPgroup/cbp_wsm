#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script creates calib septic wdms '
    echo '  It may replace wdms already in use. '
    echo '  Check the script to make sure that the variables are correctly'
    echo '  set before continuing'
    echo ' '
    echo ' To make this script run, type: create_scenario_septic_wdms_from_scenario_csv.csh GO'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' running this script creates calib septic wdms '
      echo '  It may replace wdms already in use. '
      echo '  Check the script to make sure that the variables are correctly'
      echo '  set before continuing'
      echo ' '
      echo ' To make this script run, type: create_scenario_septic_wdms_from_scenario_csv.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../../fragments/set_tree

  mkdir -p ../../../tmp/scratch/temp$$
  cd ../../../tmp/scratch/temp$$

############ USER DEFINED VARIABLES, SET THESE BELOW APPROPRIATELY
  set code = $tree/code/bin/create_septic_wdms_from_scenario_csv
  set year1 = 1984
  set year2 = 2005
 set SepticScenario =    2010WIP25BWENMN091313
  set DataFile =  septic_2010WIP25BWENMN091313.csv
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
  echo 'This dataset,' ${SepticScenario} > $notefile
  echo ' was created by' $user >> $notefile
  echo ' ' >> $notefile
  echo ' on' >> $notefile
  date >> $notefile
  echo ' ' >> $notefile
  echo 'Using the code' >> $notefile
  echo $code >> $notefile
  echo ' ' >> $notefile
  echo 'and the data in' >> $notefile
  echo 'input/unformatted/septic/'${DataFile} >> $notefile
  

  cd ../
  rm -r temp$$
