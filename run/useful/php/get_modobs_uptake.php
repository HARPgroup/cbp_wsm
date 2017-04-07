<?php

include('./config.php');

if (isset($_GET['projectid'])) {
   $projectid = $_GET['projectid'];
}



if (isset($_POST['projectid'])) {
   $projectid = $_POST['projectid'];
   $actiontype = $_POST['actiontype'];
   $infiles = $_POST['infiles'];
} else {
   $infiles = array();
}

if (count($argv)) {
   $scenid = $argv[1];
   $scname = $argv[2];
   $luname = $argv[3];
   $constit = $argv[4];
   $outfile = $argv[5];
   $minyear = $argv[6];
   $maxyear = $argv[7];
   $filename = 'php://stdout';
}


/*
print("<form action='$_SERVER['PHP_SELF']' method='Post'>");

print("<b>Select Files to Import Model Calibrated Uptakes: </b><br>");
fileMultiSelectedForm('infiles',$indir,'',10,$infiles);
showHiddenField('projectid',$projectid);

showSubmitButton('submit','Parse Model Uptakes');
print("</form>");

*/

switch ($constit) {
   case 'totn':
   $constitcol = 'uptake_n';
   break;

   case 'totp':
   $constitcol = 'uptake_p';
   break;

   default:
   $constitcol = 'uptake_n';
   break;
}

$listobject->querystring = "create temp table temp_mod_vs_obs as ";
$listobject->querystring .= " select a.landseg, a.annual_uptake as mod_uptake, a.thisyear, a.constit,  ";
$listobject->querystring .= "    CASE ";
$listobject->querystring .= "    WHEN b.agc_uptake is null THEN -9 ";
$listobject->querystring .= "    ELSE b.agc_uptake ";
$listobject->querystring .= "    END as agc_uptake ";
$listobject->querystring .= " from scen_model_uptake as a left join ";
$listobject->querystring .= "    (select a.stcofips, a.luname, a.thisyear, c.shortname as constit, ";
$listobject->querystring .= "        (a.uptk_rmvd_ratio * a.yld_pct * b.$constitcol ) as agc_uptake ";
$listobject->querystring .= "     from nass_reported_ylds as a, scen_subsheds as b, pollutanttype as c  ";
$listobject->querystring .= "     where c.shortname = '$constit' ";
$listobject->querystring .= "        and a.pollutantid = c.typeid ";
$listobject->querystring .= "        and a.luname = '$luname' ";
$listobject->querystring .= "        and b.luname = '$luname' ";
$listobject->querystring .= "        and b.scenarioid = $scenid ";
$listobject->querystring .= "        and a.stcofips = b.subshedid ";
$listobject->querystring .= "        and a.thisyear = b.thisyear ";
$listobject->querystring .= "    ) as b ";
$listobject->querystring .= "    on ( a.subshedid = b.stcofips ";
$listobject->querystring .= "    and a.thisyear = b.thisyear ) ";
$listobject->querystring .= " where a.constit = '$constit' ";
#$listobject->querystring .= "   and a.thisyear = b.thisyear ";
$listobject->querystring .= "    and a.thisyear >= $minyear ";
$listobject->querystring .= "    and a.thisyear <= $maxyear ";
$listobject->querystring .= "    and a.model_scen = '$scname' ";
$listobject->querystring .= "    and a.luname = '$luname' ";
$listobject->querystring .= "    and a.constit = '$constit' ";
$listobject->querystring .= "    and a.scenarioid = $scenid ";

$listobject->performQuery();
#print(".");
#print("$listobject->querystring \n");

$listobject->querystring = "create temp table temp_ordermodobs as ( ";
$listobject->querystring .= " select landseg, constit, 'modeled' as source, mod_uptake as uptake, thisyear ";
$listobject->querystring .= " from temp_mod_vs_obs ";
$listobject->querystring .= " order by landseg, source ";
$listobject->querystring .= " ) UNION ( ";
$listobject->querystring .= " select landseg, constit, 'observed' as source, agc_uptake as uptake, thisyear ";
$listobject->querystring .= " from temp_mod_vs_obs ";
$listobject->querystring .= " order by landseg, source ";
$listobject->querystring .= " ) ";

$listobject->performQuery();
#print(".");
#print("$listobject->querystring \n");

# set appropriate null values
$listobject->querystring = "create temp table temp_ordermodobs as ( ";
$listobject->querystring .= " select landseg, constit, 'modeled' as source, mod_uptake as uptake, thisyear ";
$listobject->querystring .= " from temp_mod_vs_obs ";
$listobject->querystring .= " order by landseg ";
$listobject->querystring .= " ) UNION ( ";
$listobject->querystring .= " select landseg, constit, 'observed' as source, agc_uptake as uptake, thisyear ";
$listobject->querystring .= " from temp_mod_vs_obs ";
$listobject->querystring .= " order by landseg ";
$listobject->querystring .= " ) ";

$listobject->performQuery();

# tro debug
#$listobject->querystring = "select * from temp_ordermodobs limit 10 ";
#$listobject->performQuery();
#$listobject->showList();

$ctstring = doGenericCrossTab($listobject, 'temp_ordermodobs', 'landseg, source', 'thisyear', 'uptake', 1);
$listobject->querystring = "create temp table temp_modobs_cross as $ctstring ";
#print("$listobject->querystring \n");
$listobject->performQuery();

$listobject->querystring = "select * from temp_modobs_cross order by landseg, source ";
#print("$listobject->querystring \n");
$listobject->performQuery();
#$listobject->showlist();
$thisarray = $listobject->queryrecords;


$colnames = array(array_keys($listobject->queryrecords[0]));
putDelimitedFile($outfile,$colnames,',',1,'unix');
putDelimitedFile($outfile,$thisarray,',',1,'unix');

?>