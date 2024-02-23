#!/bin/csh

source fragments/set_landuse

foreach line ( "`cat ../input/scenario/river/ps/PS_8514_20170423/cso_lrsegs.csv`" )
   set data = `echo $line:q | sed 's/,/ /g'`
   foreach lu ( $perlnds $implnds )
      if ( ! -e ../output/bay/aveann/P620170331WQg_S1/$data[3]_to_$data[2]_${lu}_1985-2014.ave ) then
        echo $data[3] $data[2] $lu
        cp -vip ../output/bay/aveann/P620170331WQg_S1/N42107_to_SL1_1730_1700_${lu}_1985-2014.ave ../output/bay/aveann/P620170331WQg_S1/$data[3]_to_$data[2]_${lu}_1985-2014.ave
      endif
   end
end
