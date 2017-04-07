#!/bin/csh
  set fnam = allfall.ps
  set fnam2 = allcalib.ps

  if (-e $fnam) then
    rm $fnam
  endif
  if (-e $fnam2) then
    rm $fnam2
  endif

  set falllines = ( SL9_2720_0001 PM7_4820_0001 JL7_7100_7030 RU5_6030_0001 XU3_4650_0001 YM4_6620_0003 YP4_6720_6750 JA5_7480_0001 EM2_3980_0001 )

  foreach seg ($falllines)
    cat ${seg}*.PS >> $fnam
  end

  cp $fnam $fnam2
  set basins = ( SU SW SJ SL PU PS PM PL JU JL JB JA RU RL XU XL YM YP EU EM EL WU WM OR OD NR MN TU BS GY DE)
  foreach basin ($basins)
    cat ${basin}*.PS >> $fnam2
  end

