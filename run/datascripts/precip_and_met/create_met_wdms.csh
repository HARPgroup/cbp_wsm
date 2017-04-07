#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script creates new meteorology wdms.  It may replace wdms'
    echo '  already in use. Check the script to make sure that the variables are correctly'
    echo '  set before continuing'
    echo ' '
    echo ' To make this script run, type: create_met_wdms.csh GO'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' running this script creates new meteorology wdms.  It may replace wdms'
      echo '  already in use. Check the script to make sure that the variables are correctly'
      echo '  set before continuing'
      echo ' '
      echo ' To make this script run, type: create_met_wdms.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../../fragments/set_tree

######## SET THE FOLLOWING VARIABLES CORRECTLY AND DOUBLE CHECK
  set code = $tree/code/bin/met_84_05
  set metscen = f8405
  set basin = 'all'
######### NO MORE SPECIFICATION BELOW THIS LINE

  mkdir -p ../../../tmp/scratch/temp$$/
  cd ../../../tmp/scratch/temp$$/

  mkdir -p $tree/input/scenario/climate/met/$metscen/

  source $tree/config/seglists/${basin}.land

  if (-e problem) then
    rm problem
  endif

  foreach seg ($segments)
    cp $tree/config/blank_wdm/blank_met.wdm met_${seg}.wdm 
    echo $seg | $code
    mv met_${seg}.wdm $tree/input/scenario/climate/met/$metscen/
    if (-e problem) then
      cat problem
      exit
    endif
  end

  if (-e problem) then
    rm problem
  endif

######### self-documentation
  set notefile =  $tree/input/scenario/climate/met/$metscen/AutoMetNotes
  if (-e $notefile) then
    rm $notefile
  endif
  echo 'This dataset,' ${metscen} > $notefile
  echo ' was created by' $user >> $notefile
  echo ' ' >> $notefile
  echo ' on' >> $notefile
  date >> $notefile
  echo ' ' >> $notefile
  echo ' using the data in' >> $notefile
  echo $tree/input/unformatted/p4met/ >> $notefile
  echo 'Using the code' >> $notefile
  echo $code >> $notefile

  cd ../
  rm -r temp$$

