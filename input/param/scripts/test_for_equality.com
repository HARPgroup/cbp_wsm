#!/bin/csh

 set module = $argv[1]
 set scen = $argv[2]

 source ../set_landuse

 foreach lu1 ($perlnds)

   echo 'checking for matches on land use:  ',$lu1
   foreach lu2 ($perlnds)

     diff $lu1/$scen/${module}.csv $lu2/$scen/${module}.csv >temp$$

     if (-z temp$$) then
       echo '    ',$lu1,' matches ',$lu2,' for ',$module,' in ',$scen
     endif
     rm temp$$

   end
 end

