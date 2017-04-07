#!/bin/csh

 set copyfrom = $argv[1]
 set copyto = $argv[2]

 source ../../../run/fragments/set_landuse
######### save this list, it is the list of VA segments that can not be calibrated without lower VA data
 set segments = ( A51093 A51095 A51103 A51133 A51149 A51181 A51199 A51550 A51650 A51670	A51700	A51710	A51735	A51740	A51830 )

 if (-e cpsome.edp) then
   rm cpsome.edp
 endif

 foreach seg ($segments)
   echo "g/"$seg"/d" >> cpsome.edp
 end
 echo "g/^end,/d" >> cpsome.edp
 echo "w" >> cpsome.edp
 echo "q" >> cpsome.edp

 echo "end" > endline

 foreach lu ($perlnds )

   ed ../$lu/$copyto/PWATER.csv < cpsome.edp

   if (-e temp$$) then
     rm temp$$
   endif

   foreach seg ($segments)
     grep $seg ../$lu/$copyfrom/PWATER.csv >> temp$$
   end

   cat ../$lu/$copyto/PWATER.csv temp$$ endline > temp2$$

   mv temp2$$ ../$lu/$copyto/PWATER.csv

 end

 foreach lu ($implnds )

   ed ../$lu/$copyto/IWATER.csv < cpsome.edp

   if (-e temp$$) then
     rm temp$$
   endif

   foreach seg ($segments)
     grep $seg ../$lu/$copyfrom/IWATER.csv >> temp$$
   end

   cat ../$lu/$copyto/IWATER.csv temp$$ endline > temp2$$

   mv temp2$$ ../$lu/$copyto/IWATER.csv

 end

 foreach lu (common)

   ed ../$lu/$copyto/land_evap.csv < cpsome.edp

   if (-e temp$$) then
     rm temp$$
   endif

   foreach seg ($segments)
     grep $seg ../$lu/$copyfrom/land_evap.csv >> temp$$
   end

   cat ../$lu/$copyto/land_evap.csv temp$$ endline > temp2$$

   mv temp2$$ ../$lu/$copyto/land_evap.csv

 end

 rm temp$$
