#!/bin/csh

### all p5 basins
#  set basins = ( S P E J W O D M N X Y T R B G K )

### all CB watershed basins
  set basins = ( S P E J W X Y R VAcapes )

### all CB watershed basins in efficient divisions
#  set basins = ( E P_AFL SU J P_BFL SL SW W X SJ Y R S_without_SC VAcapes )

  set scenarios = ( p52_NA858f p52_NA85df p52_NA02df p52_NA10df p52_NA10cf p52_NA108f )
  set scenarios = ( p52_NA858f p52_NA85df p52_NA02df p52_NA10df p52_NA10cf p52_NA108f )
  set scenarios = ( p52_NA858f p52_NA85df p52_NA02df p52_NA10df p52_NA10cf p52_NA108f p52_E3858f p52_E385df p52_E302df p52_E310df p52_E310cf p52_E3108f )
  set scenarios = ( pr1985LA p52_2002LA )
  set scenarios = ( p52_NA85cf p52_NA02cf p52_NA028f p52_E385cf p52_E302cf p52_E3028f )
  set scenarios = ( p52_NA85cf p52_NA028f )
  set scenarios = ( p52An p52_NA858f p52_NA85df p52_NA02df p52_NA10df p52_NA10cf p52_NA108f p52_E3858f p52_E385df p52_E302df p52_E310df p52_E310cf p52_E3108f p52_NA85cf p52_NA02cf p52_NA028f p52_E385cf p52_E302cf p52_E3028f )
  set scenarios = ( p52_TS2 )
  set scenarios = (p52_NA85df p52_NA02df p52_NA10df p52_NA85cf p52_NA02cf p52_NA858f p52_NA028f p52_NA10cf p52_NA108f pr2002 p52_TS p52_E302df p52_E385df p52_E302cf p52_E3028f p52_E310cf p52_E385cf p52_E3858f p52_E3108f pr85_half)


  foreach basin ($basins)
    foreach scenario ($scenarios)

#      sbatch run_scenario_river.csh $scenario $basin 
    end
  end

  foreach scenario ($scenarios)
#    sbatch sum_all_basins.csh $scenario seg 1991 2000
#    sbatch sum_all_basins.csh $scenario res 1991 2000
    echo $scenario
    remove_river_directories.csh $scenario
  end
