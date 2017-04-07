#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script creates calib septic wdms '
    echo '  It may replace wdms already in use. '
    echo '  Check the script to make sure that the variables are correctly'
    echo '  set before continuing'
    echo ' '
    echo ' To make this script run, type: create_year_scenario_septic_wdms_from_calib.csh GO'
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
      echo ' To make this script run, type: create_year_scenario_septic_wdms_from_calib.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../../fragments/set_tree

  mkdir -p ../../../tmp/scratch/temp$$
  cd ../../../tmp/scratch/temp$$

############ USER DEFINED VARIABLES, SET THESE BELOW APPROPIRATELY
  set code = $tree/code/bin/create_year_scenario_septic_wdms_from_calib
  set year1 = 1984
  set year2 = 2005
  set sepscen = p52_1985
  set scenyear = 1985
  set dfnam = septic_p52cal_all.csv
############ END OF USER VARIABLES

######## make directory
  mkdir -p $tree/input/scenario/river/septic/$sepscen/

####### run the code
  if (-e problem) then
    rm problem
  endif

  echo $year1 $year2 $dfnam $sepscen $scenyear | $code

  if (-e problem) then
    cat problem
    exit
  endif

######### self-documentation
  set notefile =  $tree/input/scenario/river/septic/$sepscen/AutoNotes
  if (-e $notefile) then
    rm $notefile
  endif
  echo 'This dataset,' ${sepscen} > $notefile
  echo ' was created by' $user >> $notefile
  echo ' ' >> $notefile
  echo ' on' >> $notefile
  date >> $notefile
  echo ' ' >> $notefile
  echo 'Using the code' >> $notefile
  echo $code >> $notefile
  echo ' ' >> $notefile
  echo 'and the data in' >> $notefile
  echo 'input/unformatted/septic/'${dfnam} >> $notefile
  echo ' ' >> $notefile
  echo ' using only the data for the year '$scenyear >> $notefile
  

  cd ../
  rm -r temp$$
