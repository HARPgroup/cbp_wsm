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
   $projectid = $argv[1];
   $scenid = $argv[2];
   $scname = $argv[3];
   $thisfile = $argv[4];
   $debug = $argv[5];
}


/*
print("<form action='$_SERVER['PHP_SELF']' method='Post'>");

print("<b>Select Files to Import Model Calibrated Uptakes: </b><br>");
fileMultiSelectedForm('infiles',$indir,'',10,$infiles);
showHiddenField('projectid',$projectid);

showSubmitButton('submit','Parse Model Uptakes');
print("</form>");

*/



list($headerline) = createDBFromCSV($listobject,"$thisfile",'uptake_test', 255, 1, $debug);

print("Finished Parsing $thisfile\n");

/*
$listobject->querystring = "delete from scen_model_uptake where oid in ";
$listobject->querystring .= " (select a.oid ";
$listobject->querystring .= "     from scen_model_uptake as a, ";
$listobject->querystring .= "        uptake_test as b";
$listobject->querystring .= "     where a.landseg = b.landseg ";
$listobject->querystring .= "        and a.thisyear = b.thisyear ";
$listobject->querystring .= "        and a.model_scen = '$scname' ";
$listobject->querystring .= "        and a.luname = b.luname ";
$listobject->querystring .= "        and a.scenarioid = b.scenarioid ) ";
$listobject->querystring .= "    and constit in (select constit ";
$listobject->querystring .= "     from uptake_test ";
$listobject->querystring .= "     group by constit ) ";
*/

$listobject->querystring = "  delete from scen_model_uptake ";
$listobject->querystring .= " where scen_model_uptake.scenarioid = $scenid ";
$listobject->querystring .= "    and scen_model_uptake.landseg = uptake_test.landseg ";
$listobject->querystring .= "    and scen_model_uptake.thisyear = uptake_test.thisyear ";
$listobject->querystring .= "    and scen_model_uptake.model_scen = '$scname' ";
$listobject->querystring .= "    and scen_model_uptake.luname = uptake_test.luname ";
$listobject->querystring .= "    and constit in (select constit ";
$listobject->querystring .= "       from uptake_test ";
$listobject->querystring .= "       group by constit ) ";

$listobject->performQuery();
print(".");
if ($debug) { print("$listobject->querystring ; \n"); }

# tro debug
#$listobject->querystring = "select constit from uptake_test group by constit  ";
#$listobject->performQuery();
#$listobject->showList();

$listobject->querystring = " insert into scen_model_uptake ($headerline) ";
$listobject->querystring .= " select $headerline from uptake_test";

$listobject->performQuery();
if ($debug) { print("$listobject->querystring ; \n"); }
print("Deleting Old Records these river segments. ");


$listobject->querystring = "delete from calib_uptake where oid in ";
$listobject->querystring .= " (select a.oid ";
$listobject->querystring .= "     from calib_uptake as a, ";
$listobject->querystring .= "        uptake_test as b";
$listobject->querystring .= "     where a.subshedid = b.subshedid ";
$listobject->querystring .= "        and a.thisyear = b.thisyear ";
$listobject->querystring .= "        and a.luname = b.luname ";
$listobject->querystring .= "        and a.model_scen = '$scname' ";
$listobject->querystring .= "        and b.model_scen = '$scname' ";
$listobject->querystring .= "        and a.scenarioid = b.scenarioid ) ";
$listobject->querystring .= "    and pollutantname in (select constit ";
$listobject->querystring .= "     from uptake_test as b";
$listobject->querystring .= "     group by constit ) ";


if ($debug) { print("$listobject->querystring ; \n"); }
$listobject->performQuery();
print("Finished Deleting. \n");
print(".");

print("Querying LR Segs. ");
$listobject->querystring = " create temp table lrinfo as  ";
$listobject->querystring .= "   select subshedid, landseg, riverseg, luname, luarea, thisyear, scenarioid  ";
$listobject->querystring .= "   from scen_lrsegs  ";
$listobject->querystring .= "   where scenarioid = $scenid ";
$listobject->querystring .= "      and luarea > 0 ";
$listobject->querystring .= "      and luname in (select luname from uptake_test group by luname) ";
$listobject->querystring .= "      and subshedid in (select subshedid from uptake_test group by subshedid) ";
$listobject->querystring .= "   order by landseg, luname, thisyear";
if ($debug) { print("$listobject->querystring ; \n"); }
$listobject->performQuery();
print("Found LR Segs. \n");

print("Summarizing LR Segs. ");
$listobject->querystring = " create temp table lrsuminfo as  ";
$listobject->querystring .= "   select subshedid, landseg, luname, avg(luarea) as luarea  ";
$listobject->querystring .= "   from lrinfo  ";
$listobject->querystring .= "   group by subshedid, landseg, luname";
if ($debug) { print("$listobject->querystring ; \n"); }
$listobject->performQuery();
print("Finished LR Segs. \n");

print("Querying New Uptake Info. ");
$listobject->querystring = " create temp table scupinfo as ";
$listobject->querystring .= "   select b.* from scen_model_uptake as b, ";
$listobject->querystring .= "      (select subshedid, luname, thisyear ";
$listobject->querystring .= "       from uptake_test ";
$listobject->querystring .= "       group by subshedid, luname, thisyear) as c";
$listobject->querystring .= "   where b.model_scen = '$scname'  ";
$listobject->querystring .= "      and b.scenarioid = $scenid ";
$listobject->querystring .= "      and c.subshedid = b.subshedid ";
$listobject->querystring .= "      and c.luname = b.luname ";
$listobject->querystring .= "      and c.thisyear = b.thisyear ";
$listobject->querystring .= "   order by b.landseg, b.luname, b.thisyear; ";
if ($debug) { print("$listobject->querystring ; \n"); }
$listobject->performQuery();
print("Found New Uptake Info. \n");

print("Inserting New Uptake Records for scenario: $scname. ");
$listobject->querystring = " insert into calib_uptake (subshedid, luname, luarea, thisyear, scenarioid, ";
$listobject->querystring .= "   pollutantname, max_uptake, agc_uptake, agc_pct, mod_uptake, ";
$listobject->querystring .= "   mod_eof, model_vol, model_lo_return, ";
$listobject->querystring .= "   min_lseguptk, max_lseguptk, mod_pct, agc_rept_pct, model_scen)";
$listobject->querystring .= " select  b.subshedid, b.luname, c.luarea, b.thisyear, $scenid,";
$listobject->querystring .= "    b.constit, c.uptake_n,";
$listobject->querystring .= "    CASE ";
$listobject->querystring .= "    WHEN a.agc_uptake is not null THEN a.agc_uptake ";
$listobject->querystring .= "    WHEN ((a.agc_uptake is null) AND (d.agc_uptake is null)) THEN 0.0 ";
$listobject->querystring .= "    ELSE d.agc_uptake ";
$listobject->querystring .= "    END as agc_uptake, ";
$listobject->querystring .= "    CASE ";
$listobject->querystring .= "    WHEN ( (a.agc_uptake is not null) and ( c.uptake_n > 0) ) THEN a.agc_uptake/c.uptake_n ";
$listobject->querystring .= "    WHEN ((a.agc_uptake is null) AND (d.agc_uptake is null)) THEN 0.0 ";
$listobject->querystring .= "    ELSE d.agc_uptake/c.uptake_n ";
$listobject->querystring .= "    END as agc_pct, ";
$listobject->querystring .= "    b.mod_uptake, ";
$listobject->querystring .= "    b.mod_eof, b.mod_vol, mod_loret, ";
$listobject->querystring .= "    b.min_lseguptk, b.max_lseguptk, ";
$listobject->querystring .= "    CASE ";
$listobject->querystring .= "    WHEN c.uptake_n > 0 THEN b.mod_uptake/c.uptake_n ";
$listobject->querystring .= "    ELSE 0.0 ";
$listobject->querystring .= "    END as mod_pct, ";
# modified to use new NASS data
#$listobject->querystring .= "    a.reparea/a.croparea as agc_rept_pct, ";
# NASS data does not include how much is non-reported, so we simply use number of acres
$listobject->querystring .= "    CASE ";
$listobject->querystring .= "    WHEN a.reparea is not null THEN a.reparea ";
$listobject->querystring .= "    WHEN ((a.agc_uptake is null) AND (d.agc_uptake is null)) THEN 0.0 ";
$listobject->querystring .= "    ELSE d.reparea ";
$listobject->querystring .= "    END as agc_rept_pct, ";
$listobject->querystring .= "    '$scname' ";
$listobject->querystring .= " from  ";
$listobject->querystring .= "  (  select c.subshedid, b.luname, b.thisyear, b.constit, ";
$listobject->querystring .= "     CASE  ";
$listobject->querystring .= "        WHEN a.thisyear is not null THEN  ";
$listobject->querystring .= "           sum(a.luarea * b.annual_uptake)/sum(a.luarea) ";
$listobject->querystring .= "        ELSE sum(c.luarea * b.annual_uptake)/sum(c.luarea) ";
$listobject->querystring .= "     END as mod_uptake, ";
$listobject->querystring .= "     CASE  ";
$listobject->querystring .= "        WHEN a.thisyear is not null THEN  ";
$listobject->querystring .= "           sum(a.luarea * b.annual_eof)/sum(a.luarea) ";
$listobject->querystring .= "        ELSE sum(c.luarea * b.annual_eof)/sum(c.luarea) ";
$listobject->querystring .= "     END as mod_eof, ";
$listobject->querystring .= "     CASE  ";
$listobject->querystring .= "        WHEN a.thisyear is not null THEN  ";
$listobject->querystring .= "           sum(a.luarea * b.annual_vol)/sum(a.luarea) ";
$listobject->querystring .= "        ELSE sum(c.luarea * b.annual_vol)/sum(c.luarea) ";
$listobject->querystring .= "     END as mod_vol, ";
$listobject->querystring .= "     CASE  ";
$listobject->querystring .= "        WHEN a.thisyear is not null THEN  ";
$listobject->querystring .= "           sum(a.luarea * b.annual_lo_return)/sum(a.luarea) ";
$listobject->querystring .= "        ELSE sum(c.luarea * b.annual_lo_return)/sum(c.luarea) ";
$listobject->querystring .= "     END as mod_loret, ";
$listobject->querystring .= "     min(b.annual_uptake) as min_lseguptk,  ";
$listobject->querystring .= "     max(b.annual_uptake) as max_lseguptk ";
$listobject->querystring .= "     from scupinfo as b left outer join lrinfo as a ";
$listobject->querystring .= "        on ( a.landseg = b.landseg ";
$listobject->querystring .= "           and a.thisyear = b.thisyear  ";
$listobject->querystring .= "           and a.luname = b.luname ";
$listobject->querystring .= "        )  ";
$listobject->querystring .= "     left outer join lrsuminfo as c ";
$listobject->querystring .= "        on ( b.landseg = c.landseg ";
$listobject->querystring .= "           and b.luname = c.luname ";
$listobject->querystring .= "        )  ";
$listobject->querystring .= "     group by c.subshedid, b.luname, a.thisyear, b.thisyear, b.constit ";
# modified to use new NASS data
#$listobject->querystring .= "   ) as b left outer join agc_reported_ylds as a ";
$listobject->querystring .= "   ) as b left outer join nass_reported_ylds as a ";
$listobject->querystring .= " on ( a.stcofips = b.subshedid ";
$listobject->querystring .= "         and a.luname = b.luname ";
$listobject->querystring .= "         and a.thisyear = b.thisyear ";
$listobject->querystring .= "    ) left outer join scen_subsheds as c ";
$listobject->querystring .= " on ( b.subshedid = c.subshedid";
$listobject->querystring .= "         and c.scenarioid = $scenid ";
$listobject->querystring .= "         and b.luname = c.luname";
$listobject->querystring .= "         and b.thisyear = c.thisyear ";
$listobject->querystring .= "   ) left outer join agc_reported_ylds as d ";
$listobject->querystring .= " on ( d.stcofips = b.subshedid ";
$listobject->querystring .= "         and d.luname = b.luname ";
$listobject->querystring .= "         and d.thisyear = b.thisyear ";
$listobject->querystring .= "    ) ";

if ($debug) { print("$listobject->querystring ; \n"); }
$listobject->performQuery();
print("Finished Inserting. \n");


?>
