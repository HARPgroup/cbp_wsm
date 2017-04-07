#!/bin/csh
 set froms = ( SU SW SJ CP RU MN JA YP XU GY TU YM BS OO )

 set to = p5186

 foreach from ($froms)
  copy_river_parameters.csh $from $to $from
 end
