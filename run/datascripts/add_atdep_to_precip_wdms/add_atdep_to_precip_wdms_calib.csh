#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script adds atmospheric deposition to the precipitation wdms.'
    echo '  it will not modify the precipitation and will replace any previously loaded'
    echo '  atmospheric deposition'
    echo ' '
    echo ' To make this script run, type: add_atdep_to_precip_wdms_calib.csh GO'
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
      echo ' To make this script run, type: add_atdep_to_precip_wdms_calib.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../../fragments/abs_tree

######## SET THE FOLLOWING VARIABLES CORRECTLY AND DOUBLE CHECK
  set code = $tree/code/bin/add_atdep_grimm07_CMAQ12_ORG_PO4
  set oldpradscen = f611ns
  set newpradscen = f611nsA902
  set basin = 'noF_all'
  set year1 = 1984
  set year2 = 2005
  set CMAQcal = dry_2009_02_09
  set directory2cal = Chesapeake.base2002.12km
  set longnamecal = agg.CCTM_M2b_v32soa_v3.4beta3_oceancl2_12km.dep.monthly.colrow
######### NO MORE SPECIFICATION BELOW THIS LINE

  mkdir -p ../../../tmp/scratch/temp$$/
  cd ../../../tmp/scratch/temp$$/

  source $tree/config/seglists/${basin}.land

  if (-e problem) then
    rm problem
  endif

  mkdir -p $tree/input/scenario/climate/prad/$newpradscen/
  mkdir -p $tree/input/scenario/climate/prad/$newpradscen/txt/

  set summaryfile = $tree/input/scenario/climate/prad/$newpradscen/allatdep_${newpradscen}.csv
  
  if (-e $summaryfile) then
    rm $summaryfile
  endif

  foreach seg ($segments)
    cp $tree/input/scenario/climate/prad/$oldpradscen/prad_${seg}.wdm .
    echo $seg
    echo $seg $year1 $year2 $CMAQcal $directory2cal $longnamecal | $code >> $summaryfile
    if (-e problem) then
      cat problem
      exit
    endif
    mv prad_${seg}.wdm $tree/input/scenario/climate/prad/$newpradscen/
    mv *.csv $tree/input/scenario/climate/prad/$newpradscen/txt/
  end

######### self-documentation
  set notefile =  $tree/input/scenario/climate/prad/$newpradscen/AutoAtdepNotes
  if (-e $notefile) then
    rm $notefile
  endif
  echo 'This dataset,' ${newpradscen} > $notefile
  echo ' was created by' $user >> $notefile
  echo ' ' >> $notefile
  echo ' on' >> $notefile
  date >> $notefile
  echo ' ' >> $notefile
  echo ' from the old data set ' $oldpradscen >> $notefile
  echo ' by replacing the old atmospheric deposition with the deposition in' >> $notefile
  echo $tree/input/unformatted/atdep/CMAQ/$CMAQcal/$directory2cal/ >> $notefile
  echo 'Using the code' >> $notefile
  echo $code >> $notefile

  cd ../
  rm -r temp$$


