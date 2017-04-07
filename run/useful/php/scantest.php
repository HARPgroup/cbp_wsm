<?php

$fstr = '%5[^\n]%5[^\n]%10[^\n]%10[^\n]%10f%10f%10f%10f%10f';

$outarr = sscanf ( '    1                                 0.  0.34E-06        0.  4.662167  15183.03',$fstr ) ;

print_r($outarr);

?>
