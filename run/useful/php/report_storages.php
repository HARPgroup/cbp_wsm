<html>
<body>



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

# check to see if this has been called at the command line
if (count($argv)) {
   $projectid = $argv[1];
   $thisfile = $argv[2];
   $outfile = $argv[3];
   $runid = $argv[4];
   $model_scen = $argv[5];
}


/*
print("<form action='$_SERVER['PHP_SELF']' method='Post'>");

print("<b>Select Files to Import Model Calibrated Uptakes: </b><br>");
fileMultiSelectedForm('infiles',$indir,'',10,$infiles);
showHiddenField('projectid',$projectid);

showSubmitButton('submit','Parse Model Uptakes');
print("</form>");

*/



list($headerline) = createDBFromCSV($listobject,"$thisfile",'storage_test', 255, 1, 0);

print("Finished Parsing $thisfile ... exporting to database\n");

$listobject->querystring = "delete from scen_model_storage where oid in ";
$listobject->querystring .= " (select a.oid ";
$listobject->querystring .= "     from scen_model_storage as a, ";
$listobject->querystring .= "        storage_test as b";
$listobject->querystring .= "     where a.landseg = b.landseg ";
$listobject->querystring .= "        and a.thisyear = b.thisyear ";
$listobject->querystring .= "        and a.luname = b.luname ";
$listobject->querystring .= "        and a.scenarioid = b.scenarioid  ";
$listobject->querystring .= "        and a.model_scen = '$model_scen' ) ";

$listobject->performQuery();
print(".");
#print("$listobject->querystring \n");

$listobject->querystring = " insert into scen_model_storage ($headerline, model_scen) ";
$listobject->querystring .= " select $headerline, '$model_scen' from storage_test";

$listobject->performQuery();
#print("$listobject->querystring \n");

# output file name

if (strlen($outfile) > 0) {
   print("Finished exporting to database ... outputting to file $outfile\n");
   $ctsql = doGenericCrossTab($listobject, 'storage_test' , 'landseg,luname,constit,layer', 'thisyear', 'thisvalue', 1);

   $listobject->querystring = $ctsql;
   #print("Query: $listobject->querystring \n\n");
   $listobject->performQuery();

   $colnames = array(array_keys($listobject->queryrecords[0]));

   putDelimitedFile("$outfile", $colnames, ',',1,'unix');
   putDelimitedFile("$outfile", $listobject->queryrecords, ',',0,'unix');
}



?>

</body>
</html>
