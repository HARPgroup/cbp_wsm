#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script creates zero diversion wdms by copying the blank wdm'
    echo '  for each river.  It may replace wdms already in use. '
    echo '  Check the script to make sure that the variables are correctly'
    echo '  set before continuing'
    echo ' '
    echo ' To make this script run, type: create_div_wdms.csh GO'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' running this script creates zero diversion wdms by copying the blank wdm'
      echo '  for each river.  It may replace wdms already in use. '
      echo '  Check the script to make sure that the variables are correctly'
      echo '  set before continuing'
      echo ' '
      echo ' To make this script run, type: create_div_wdms.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../../fragments/set_tree

  mkdir -p ../../../tmp/scratch/temp$$
  cd ../../../tmp/scratch/temp$$

############ USER DEFINED VARIABLES, SET THESE BELOW APPROPIRATELY
  set basin = all
  set newscen = zeros
############ END OF USER VARIABLES

######## make directory
  mkdir -p $tree/input/scenario/river/div/$newscen/

###### copy the zero wdm
  source $tree/config/seglists/${basin}.riv

  foreach seg ($segments)
    cp -v $tree/config/blank_wdm/blank_div.wdm $tree/input/scenario/river/div/$newscen/DIV_${seg}.wdm 
  end

######### self-documentation
  set notefile =  $tree/input/scenario/river/div/$newscen/AutoNotes
  if (-e $notefile) then
    rm $notefile
  endif
  echo 'This dataset,' ${newscen} > $notefile
  echo ' was created by' $user >> $notefile
  echo ' ' >> $notefile
  echo ' on' >> $notefile
  date >> $notefile
  echo ' ' >> $notefile
  echo 'by copying the blank wdm for each river seg' >> $notefile

  cd ../
  rm -r temp$$

