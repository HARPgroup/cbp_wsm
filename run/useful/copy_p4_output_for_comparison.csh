#!/bin/csh

 source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/

 mkdir -p $tree/output/river/daily/phase4/

 echo SL9_2720_0001 140 susq conow     | ../../code/bin/copy_p4.exe
 echo SL9_2490_2520 110 susq losus     | ../../code/bin/copy_p4.exe
 echo PM7_4820_0001 220 potm lopot     | ../../code/bin/copy_p4.exe
 echo XU3_4650_0001 340 potm patux     | ../../code/bin/copy_p4.exe
 echo RU5_6030_0001 230 potm rappa     | ../../code/bin/copy_p4.exe
 echo YM4_6620_0003 240 james matta    | ../../code/bin/copy_p4.exe
 echo YP4_6720_6750 260 james pamun    | ../../code/bin/copy_p4.exe
 echo JL7_7100_7030 280 james james2   | ../../code/bin/copy_p4.exe
 echo JA5_7480_0001 310 james appom    | ../../code/bin/copy_p4.exe
 echo EM2_3980_0001 770 coast choptank | ../../code/bin/copy_p4.exe
 echo PS5_4380_4370 200 potm shena     | ../../code/bin/copy_p4.exe
 echo PU6_3752_4080 740 potm midpot    | ../../code/bin/copy_p4.exe
 echo SJ6_2130_0003 100 susq junia     | ../../code/bin/copy_p4.exe
 echo SU7_0850_0730 700 susq ebsus1    | ../../code/bin/copy_p4.exe
 echo SW7_1640_0003 70 susq wbsus      | ../../code/bin/copy_p4.exe


