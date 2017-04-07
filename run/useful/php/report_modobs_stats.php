<?php

include('./config.php');
clearstatcache();

# genreral script format for change_param.com is as follows:
#A10003 - landseg
#nut - scenario
#NITR1 - hspf module
#NIT-FSTPM#1 - hspf table
#KIMAM - hspf variable
#r - type real/integer
#e - action equals/multiply/add
#1.51 - value
#s - single land use/all/perlands/implands/multiple
#15 - land use id (only valid if doing a single land use)

#$scriptdir = './sensitivity_scripts/minimob_test';

$scenarioid = $argv[1];
$scenario = $argv[2];
$basename = $argv[3];

$knikam[1] = 0.01;
$knikam[2] = 0.05;
$knikam[3] = 0.1;
$knikam[4] = 0.15;
$knikam[5] = 0.2;
$knikam[6] = 0.5;
$knikam[7] = 1.0;

$kimnikimam[1] = 0.25;
$kimnikimam[2] = 0.5;
$kimnikimam[3] = 1.0;
$kimnikimam[4] = 2.0;
$kimnikimam[5] = 5.0;


for ($i = 1;$i <= count($knikam); $i++ ) {
   for ($j = 1;$j <= count($kimnikimam); $j++) {
      print(" Handling $basename.$i.$j.csv \n");
      list($headerline) = createDBFromCSV($listobject,"$basename.$i.$j.csv",'modobs_temp', 512, 1,0);

      $kni = $knikam[$i];
      $kimni = $kimnikimam[$j];
      $listobject->querystring = "delete from modobs_stats where kni = $kni and kimni = $kimni ";
      $listobject->querystring .= " and seg in (select seg from modobs_temp) ";
      $listobject->performQuery();

      $listobject->querystring = "insert into modobs_stats (kni, kimni, seg, r, bias, nse) ";
      $listobject->querystring .= " select $kni, $kimni, seg, r, bias, nse from modobs_temp ";
      $listobject->performQuery();

      $listobject->querystring = "drop table modobs_temp ";
      $listobject->performQuery();

   }
}

# now, do the stats for each parameter of interest
# bias
$ctq = doGenericCrossTab ($listobject, 'modobs_stats' , 'kni', 'kimni', 'bias', 1, 2);

$listobject->querystring = $ctq;
$listobject->performQuery();
$thisarray = $listobject->queryrecords;

$colnames = array(array_keys($listobject->queryrecords[0]));
putDelimitedFile("$basename.bias.csv",$colnames,',',1,'unix');
putDelimitedFile("$basename.bias.csv",$thisarray,',',0,'unix');

# r-squared
$ctq = doGenericCrossTab ($listobject, 'modobs_stats' , 'seg,kni', 'kimni', 'r', 1, 2);
$listobject->querystring = $ctq;
$listobject->performQuery();
$thisarray = $listobject->queryrecords;
#print("$ctq \n");

$colnames = array(array_keys($listobject->queryrecords[0]));
putDelimitedFile("$basename.rsquare.csv",$colnames,',',1,'unix');
putDelimitedFile("$basename.rsquare.csv",$thisarray,',',0,'unix');
?>