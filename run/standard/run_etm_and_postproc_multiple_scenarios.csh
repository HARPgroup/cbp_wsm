#!/bin/csh

### all p5 basins
#  set basins = ( S P E J W O D M N X Y T R B G K )

### all CB watershed basins
#  set basins = ( S P E J W X Y R VAcapes )

### all CB watershed basins in effiient divisions
  set basins = ( E P_AFL SU J P_BFL SL SW W X SJ Y R S_without_SC VAcapes )


  set scenarios = ( p52_E3858f p52_E385df p52_E302df p52_E310df p52_E310cf p52_E3108f )


  foreach basin ($basins)
    foreach scenario ($scenarios)

      sbatch run_etm_and_land_and_dat_simultaneously.csh $scenario $basin 1991 2000

    end
  end
