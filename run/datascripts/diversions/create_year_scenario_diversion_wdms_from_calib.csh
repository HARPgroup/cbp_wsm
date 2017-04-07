#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script creates scenario diversion wdms by selecting a single year'
    echo '  from the time series.  It may replace wdms already in use. '
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
      echo ' running this script creates scenario diversion wdms by selecting a single year'
      echo '  from the time series.  It may replace wdms already in use. '
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
  set code = $tree/code/bin/create_year_scenario_diversions_from_calib
  set year1 = 1984
  set year2 = 2005
  set scenyear = 2002
  set basin = all
  set calibscen = ext05
  set newscen = div02
############ END OF USER VARIABLES

######## make directory
  mkdir -p $tree/input/scenario/river/div/$newscen/

####### run the code
  if (-e problem) then
    rm problem
  endif

  source $tree/config/seglists/${basin}.riv

  foreach seg ($segments)
    cp $tree/input/scenario/river/div/$calibscen/DIV_${seg}.wdm .
    echo DIV_${seg}.wdm $year1 $year2 $scenyear | $code
    mv DIV_${seg}.wdm $tree/input/scenario/river/div/$newscen/
    if (-e problem) then
      cat problem
      exit
    endif
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
  echo 'Using the code' >> $notefile
  echo $code >> $notefile
  echo ' ' >> $notefile
  echo 'and the data in' >> $notefile
  echo 'input/input/div/'$calibscen'/' >> $notefile
  echo ' ' >> $notefile
  echo ' using only the data for the year '$scenyear >> $notefile


  cd ../
  rm -r temp$$

