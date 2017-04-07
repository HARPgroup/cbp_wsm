#!/bin/csh

  set lu = (for pul puh)
  foreach landuse ($lu)
    run_PQUAL_optimization.csh p5186 p5186 all $landuse
  end
  
