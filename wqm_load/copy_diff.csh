#!/bin/csh

 set fnam = $argv[1]

 set good =  p5_and_ps_to_wqm57k
 set dirs = (factor_scenario_p5_and_ps_to_wqm57k p5_and_ps_to_boynton p5_and_ps_to_wqm13k p5_and_ps_to_yuepeng p5_no_ps_to_boynton quick_kluge_p5_and_ps_to_wqm57k)

 foreach dir ($dirs)
   cp -v $good/$fnam $dir/$fnam
 end
