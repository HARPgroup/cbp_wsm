#!/bin/csh

##### just the segments in the Potomac
####### this script creates the Nov 1985 storm in the Potomac, which was not caught by
######## rain gages

  if (${#argv} != 1) then
    echo ' '
    echo ' Running this script modifies the rainfall in the Potomac for Tropical Storm Juan'
    echo '  which was not adequately characterized by rain gauges'
    echo ' Running this script will increase rainfall in the selected segments'
    echo ' If you run it twice for the same dataset, it will double the effect'
    echo ' '
    echo ' If you are sure you want to run this script, check all variables in the script'
    echo ' '
    echo ' To make this script run, type: change_precip_nov_storm.csh GO'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' Running this script modifies the rainfall in the Potomac for Tropical Storm Juan'
      echo '  which was not adequately characterized by rain gauges'
      echo ' Running this script will increase rainfall in the selected segments'
      echo ' If you run it twice for the same dataset, it will double the effect'
      echo ' '
      echo ' If you are sure you want to run this script, check all variables in the script'
      echo ' '
      echo ' To make this script run, type: change_precip_nov_storm.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../../fragments/set_tree

######## SET THE FOLLOWING VARIABLES CORRECTLY AND DOUBLE CHECK
  set code = $tree/code/bin/change_precip_for_Nov1985_storm
  set oldpradscen = f611
  set newpradscen = f611ns
  set basin = 'all'
  set year1 = 1984
  set year2 = 2005
######### NO MORE SPECIFICATION BELOW THIS LINE

####################### these are the only segments that are effected by the change
  set changesegments = ( A51069 B51171 A51015 A51165 B51165 C51015 A54031 A51171 A51187 A51820 B51015 A51660 A51790 A51139 C51165 B51139 B51187 A51043 A54037 A42055 A42099 B42055 A51840 A42057 B42001 B42041 A54003 A54065 A24001 C42009 A54027 A42009 B54071 A54023 A54057 B54023 B54057 A42111 B24001 B42009 A54071 B51091 A24023 A51091 B54031 A54077 A54093 )

  mkdir -p ../../../tmp/scratch/temp$$/
  cd ../../../tmp/scratch/temp$$/

  source $tree/config/seglists/${basin}.land

  if (-e problem) then
    rm problem
  endif

  mkdir -p $tree/input/scenario/climate/prad/$newpradscen/
  mkdir -p $tree/input/scenario/climate/prad/$newpradscen/txt/

########## deal with summary files, copy old and delete the lines to change
  set summaryfile = ${tree}'/input/scenario/climate/prad/'${newpradscen}'/ann_precip_'${newpradscen}'.txt'
  cp ${tree}'/input/scenario/climate/prad/'${oldpradscen}'/ann_precip_'${oldpradscen}'.txt' $summaryfile
  if (-e ed.$$) then
    rm ed.$$
  endif
  foreach changeseg ($changesegments)
    echo 'g/'$changeseg'/d' >> ed.$$
  end
  echo 'w' >> ed.$$
  echo 'q' >> ed.$$
  ed $summaryfile < ed.$$

####### loop over all segments, copy from old scen, move to new scen, change if necessary
  foreach seg ($segments)
    cp $tree/input/scenario/climate/prad/$oldpradscen/prad_${seg}.wdm .
    foreach changeseg ($changesegments)
      if ($seg == $changeseg) then
        echo $seg
        echo $seg $year1 $year2 | $code >> $summaryfile
      endif
    end
    mv prad_${seg}.wdm $tree/input/scenario/climate/prad/$newpradscen/
    if (-e problem) then
      cat problem
      exit
    endif
  end

######### self-documentation
  set notefile =  $tree/input/scenario/climate/prad/$newpradscen/AutoPrecipNotes
  if (-e $notefile) then
    rm $notefile
  endif
  echo 'This dataset,' ${newpradscen} > $notefile
  echo ' was created by' $user >> $notefile
  echo ' ' >> $notefile
  echo ' on' >> $notefile
  date >> $notefile
  echo ' ' >> $notefile
  echo ' by copying the precip from '$oldpradscen  >> $notefile
  echo ' and changing some segments for the november 1985 storm' >> $notefile
  echo 'Using the code' >> $notefile
  echo $code >> $notefile

  cd ../
  rm -r temp$$
