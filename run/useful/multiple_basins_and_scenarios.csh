#!/bin/csh

### all p5 basins
#  set basins = ( S P E J W O D M N X Y T R B G K )

### all CB watershed basins
  set basins = ( S P E J W X Y R VAcapes )

### all CB watershed basins in efficient divisions
#  set basins = ( E P_AFL SU J P_BFL SL SW W X SJ Y R S_without_SC VAcapes )
#  set basins = ( SU SL SW SJ )


  set scenarios = ( p52_E3858f p52_E385df p52_E302df p52_E310df p52_E310cf p52_E3108f )
  set scenarios = ( p52_NA858f p52_NA85df p52_NA02df p52_NA10df p52_NA10cf p52_NA108f )
  set scenarios = ( p52_NA028f p52_NA85cf )
  set scenarios = ( p52_TS2 )


  foreach basin ($basins)
    foreach scenario ($scenarios)

#      sbatch run_etm_and_land_and_dat_simultaneously.csh $scenario $basin 1991 2000

#      sbatch run_postproc_river_aveann.csh $scenario $basin 1991 2000
      sbatch run_postproc_tf_df_del_aveann.csh $scenario $basin 1991 2000

    end
  end
