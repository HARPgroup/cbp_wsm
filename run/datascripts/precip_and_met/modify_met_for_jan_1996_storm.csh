#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script modifies the temperature of the existing met wdms.  It may replace wdms'
    echo '  already in use. Check the script to make sure that the variables are correctly'
    echo '  set before continuing'
    echo ' '
    echo ' To make this script run, type: create_precip_wdms.csh GO'
    echo ' '
    echo ' the particular segments to which this applies are listed in this script'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' running this script modifies the temperature of the existing met wdms.  It may replace wdms'
      echo '  already in use. Check the script to make sure that the variables are correctly'
      echo '  set before continuing'
      echo ' '
      echo ' To make this script run, type: create_precip_wdms.csh GO'
      echo ' '
      echo ' the particular segments to which this applies are listed in this script'
      echo ' '
      exit
    endif
  endif

  source ../../fragments/set_tree

######## SET THE FOLLOWING VARIABLES CORRECTLY AND DOUBLE CHECK
  set code = $tree/code/bin/modify_jan96_storm_temperature
  set oldmetscen = f8405xyz
  set newmetscen = fjs8405xyz
  set basin = 'all'
  set year1 = 1984
  set year2 = 2005
######### NO MORE SPECIFICATION BELOW THIS LINE

 set modsegs = ( A10001 A10003 A10005 A24001 A24009 A24011 A24015 A24017 A24019 A24023 A24029 A24035 A24037 A24039 A24041 A24045 A24047 A36003 A36007 A36015 A36017 A36023 A36025 A36043 A36051 A36053 A36065 A36067 A36069 A36077 A36095 A36097 A36101 A36107 A36109 A36123 A37001 A37005 A37009 A37033 A37067 A37077 A37081 A37131 A37135 A37145 A37157 A37169 A37171 A37181 A37185 A37189 A42009 A42013 A42015 A42021 A42023 A42025 A42027 A42033 A42035 A42037 A42043 A42047 A42057 A42061 A42063 A42065 A42067 A42069 A42075 A42079 A42081 A42083 A42087 A42093 A42097 A42105 A42107 A42109 A42111 A42113 A42115 A42117 A42119 A42127 A42131 A47091 A47163 A51001 A51003 A51005 A51007 A51009 A51011 A51015 A51017 A51019 A51021 A51023 A51025 A51027 A51029 A51031 A51033 A51035 A51036 A51037 A51041 A51043 A51045 A51047 A51049 A51051 A51053 A51057 A51061 A51063 A51065 A51067 A51069 A51071 A51073 A51075 A51077 A51079 A51081 A51083 A51085 A51087 A51089 A51091 A51093 A51095 A51097 A51099 A51101 A51103 A51105 A51107 A51109 A51111 A51113 A51115 A51117 A51119 A51121 A51125 A51127 A51131 A51133 A51135 A51137 A51139 A51141 A51143 A51145 A51147 A51149 A51153 A51155 A51157 A51159 A51161 A51163 A51165 A51167 A51169 A51171 A51173 A51175 A51177 A51179 A51181 A51183 A51185 A51187 A51191 A51193 A51195 A51197 A51199 A51515 A51520 A51530 A51540 A51550 A51570 A51580 A51590 A51595 A51600 A51620 A51630 A51640 A51650 A51660 A51670 A51678 A51680 A51683 A51685 A51690 A51700 A51710 A51720 A51730 A51735 A51740 A51750 A51760 A51770 A51775 A51790 A51800 A51810 A51820 A51830 A51840 A54003 A54023 A54027 A54031 A54037 A54057 A54063 A54065 A54071 A54077 A54093 B24001 B37005 B37009 B42009 B42015 B42027 B42035 B42079 B42081 B42119 B42131 B51003 B51009 B51015 B51017 B51019 B51023 B51035 B51061 B51067 B51071 B51077 B51079 B51091 B51105 B51113 B51125 B51139 B51141 B51157 B51161 B51163 B51165 B51167 B51169 B51171 B51173 B51185 B51187 B51191 B51197 B54023 B54031 B54057 B54071 C42009 C51015 C51071 C51165 B51195 F10005 F24001 F24009 F24015 F24017 F24019 F24023 F24029 F24035 F24037 F24039 F24041 F24045 F36003 F36007 F36015 F36025 F36101 F42009 F42013 F42021 F42027 F42033 F42035 F42043 F42061 F42069 F42075 F42079 F42081 F42087 F42097 F42107 F42115 F42117 F42119 F51001 F51003 F51005 F51009 F51011 F51015 F51017 F51019 F51023 F51033 F51041 F51043 F51045 F51053 F51057 F51061 F51069 F51071 F51079 F51085 F51087 F51091 F51095 F51099 F51101 F51107 F51113 F51121 F51125 F51131 F51137 F51139 F51145 F51147 F51149 F51153 F51157 F51159 F51161 F51163 F51165 F51171 F51177 F51179 F51181 F51187 F51193 F51199 F51530 F51540 F51550 F51580 F51630 F51650 F51660 F51670 F51700 F51710 F51730 F51735 F51740 F51760 F51790 F51800 F51810 F51820 F51830 F54003 F54023 F54027 F54031 F54037 F54057 F54063 F54065 F54071 )

 source $tree/config/seglists/${basin}.land

  mkdir -p ../../../tmp/scratch/temp$$/
  cd ../../../tmp/scratch/temp$$/

  if (-e problem) then
    rm problem
  endif

  mkdir -p $tree/input/scenario/climate/met/$newmetscen/
  mkdir -p $tree/input/scenario/climate/met/$newmetscen/txt/

  foreach seg ($segments)
    cp $tree/input/scenario/climate/met/$oldmetscen/met_${seg}.wdm .
    foreach modseg ($modsegs)
      if ($seg == $modseg) then
        echo $seg $year1 $year2 | $code 
      endif
    end
    mv met_${seg}.wdm $tree/input/scenario/climate/met/$newmetscen/
    if (-e problem) then
      cat problem
      exit
    endif
  end

######### self-documentation
  set notefile =  $tree/input/scenario/climate/met/$newmetscen/AutoMetNotes
  if (-e $notefile) then
    rm $notefile
  endif
  echo 'This dataset,' ${newmetscen} > $notefile
  echo ' was created by' $user >> $notefile
  echo ' ' >> $notefile
  echo ' on' >> $notefile
  date >> $notefile
  echo ' ' >> $notefile
  echo ' by starting with the wdms in '$oldmetscen >> $notefile
  echo ' and modifying the temperature between the 1/18/1996 and 1/21/1996 ' >> $notefile
  echo 'Using the code' >> $notefile
  echo $code >> $notefile

  cd ../
  rm -r temp$$
