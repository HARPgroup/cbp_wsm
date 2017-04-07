#!/bin/csh

 if (${#argv} != 1) then
   echo ' '
   echo 'usage: copy_AGCHEM_to_nutrient_management.csh scenario'
   echo ' '
   exit
 endif

 set scen = $argv[1]

 cd ../

 set pfiles = ( MSTLAY NITR1 NITR2 NITR3 NITR4 NITR5 NITR6 NITR7 NITR8 RORGN PHOS1 PHOS2 PHOS3 PHOS4 PHOS5 )

   mkdir -p nal/$scen
   mkdir -p nlo/$scen
   mkdir -p nhi/$scen
   mkdir -p nho/$scen
   mkdir -p nhy/$scen
   mkdir -p npa/$scen
   foreach file ($pfiles)
     cp -vp alf/$scen/${file}.csv nal/$scen/
     cp -vp lwm/$scen/${file}.csv nlo/$scen/
     cp -vp hwm/$scen/${file}.csv nhi/$scen/
     cp -vp hom/$scen/${file}.csv nho/$scen/
     cp -vp hyw/$scen/${file}.csv nhy/$scen/
     cp -vp pas/$scen/${file}.csv npa/$scen/
   end
   chmod 660 nal/$scen/*v
   chmod 660 nlo/$scen/*v
   chmod 660 nhi/$scen/*v
   chmod 660 nho/$scen/*v
   chmod 660 nhy/$scen/*v
   chmod 660 npa/$scen/*v





