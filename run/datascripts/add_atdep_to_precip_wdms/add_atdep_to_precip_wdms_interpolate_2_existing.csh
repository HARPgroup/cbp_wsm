#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script adds atmospheric deposition to the precipitation wdms.'
    echo '  it will not modify the precipitation and will replace any previously loaded'
    echo '  atmospheric deposition'
    echo ' '
    echo ' To make this script run, type: add_atdep_to_precip_wdms_year_scenario.csh GO'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' running this script adds atmospheric deposition to the precipitation wdms.'
      echo '  it will not modify the precipitation and will replace any previously loaded'
      echo '  atmospheric deposition'
      echo ' '
      echo ' To make this script run, type: add_atdep_to_precip_wdms_year_scenario.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../../fragments/abs_tree

######## SET THE FOLLOWING VARIABLES CORRECTLY AND DOUBLE CHECK
########## Frac2Scen2 is the fraction moving from scenario 1 to scenario 2
############### a zero value will produce scenario 1 and a 1.000 value will produce sceanrio 2
  set code = $tree/code/bin/add_atdep_interpolate_2_existing
  set fromscen1 = v2013_2008
  set fromscen2 = v2013_2010
  set newscen = v2013_2009
  set basin = 'all'
  set year1 = 1984
  set year2 = 2005
  set FracToScen2 = 0.5
########## END USER-DEFINED VARIABLES

  mkdir -p ../../../tmp/scratch/temp$$/
  cd ../../../tmp/scratch/temp$$/

  source $tree/config/seglists/${basin}.land

  mkdir -p $tree/input/scenario/climate/prad/$newscen/

  if (-e problem) then
    rm problem
  endif

  set summaryfile = $tree/input/scenario/climate/prad/$newscen/allatdep_${newscen}.csv

  if (-e $summaryfile) then
    rm $summaryfile
  endif

  foreach seg ($segments)
    cp $tree/input/scenario/climate/prad/$fromscen1/prad_${seg}.wdm .
    cp $tree/input/scenario/climate/prad/$fromscen2/prad_${seg}.wdm prad_${seg}.wdm2
    echo $seg
    echo $seg $year1 $year2 $FracToScen2 | $code >> $summaryfile
    if (-e problem) then
      cat problem
      exit
    endif
    mv prad_${seg}.wdm $tree/input/scenario/climate/prad/$newscen/
  end

######### self-documentation
  set notefile =  $tree/input/scenario/climate/prad/$newscen/AutoAtdepNotes
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
  echo 'The data sets in ../'$fromscen1' and ../'$fromscen2 >> $notefile
  echo ' were interpolated to produce this dataset' >> $notefile
  echo ' the resulting dataset is weighted toward '$fromscen2' by the factor '$FracToScen2 >> $notefile

  cd ../
  rm -r temp$$


