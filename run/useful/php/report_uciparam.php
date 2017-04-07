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
      $scenarioid = $argv[2];
      $thisfile = $argv[3];
      $thissection = $argv[4];
      $thisblock = $argv[5];
      $thisparam = strtoupper($argv[6]);
      $paramcol = strtolower($argv[6]);
      $landseg = $argv[7];
      $subshedid = $argv[8];
      $luname = $argv[9];
      $scname = $argv[10];
      $outfile = $argv[11];
   }

   $uciobject->ucifile = $thisfile;
   $uciobject->debug = 0;

   if (!in_array($thisblock, array_keys($uciobject->ucitables))) {
      print("$thisblock/$thisparam not found in setup tables, unable to parse. Check the file hspf.defaults.php to locate this parameter description.");
      die;
   }

   /*
   print("<form action='$_SERVER['PHP_SELF']' method='Post'>");

   print("<b>Select Files to Import Model Calibrated Uptakes: </b><br>");
   fileMultiSelectedForm('infiles',$indir,'',10,$infiles);
   showHiddenField('projectid',$projectid);

   showSubmitButton('submit','Parse Model Uptakes');
   print("</form>");

   */

   $thisinfo = parseMultiFixedWidthUCI($uciobject->ucifile, $thissection, $thisblock, $uciobject->ucitables[$thisblock]["uciformat"], $uciobject->debug);

   #print_r($thisinfo);
   #die;

   $header = array('scenarioid','subshedid','landseg','luname','paramname','paramval','model_scen','param_row');
   putDelimitedFile("$outfile",$header,",",1,'unix');

   foreach (array_keys($thisinfo) as $infoid) {

      $tbdata = $thisinfo[$infoid];

      makeUCITable($uciobject->listobject,$thisblock,$tbdata,$uciobject->ucitables,$uciobject->debug, 1);

      $tablename = $uciobject->ucitables[$thisblock]["tablename"];

      $listobject->querystring = "delete from scen_hspf_parms ";
      $listobject->querystring .= "where scenarioid = $scenarioid ";
      $listobject->querystring .= "   and subshedid = '$subshedid' ";
      $listobject->querystring .= "   and landseg = '$landseg' ";
      $listobject->querystring .= "   and paramname = '$thisparam' ";
      $listobject->querystring .= "   and luname = '$luname' ";
      $listobject->querystring .= "   and model_scen = '$scname' ";
      $listobject->querystring .= "   and param_row = '$infoid' ";

      $listobject->performQuery();
      #print("$listobject->querystring \n");

      $listobject->querystring = "insert into scen_hspf_parms (scenarioid, subshedid, ";
      $listobject->querystring .= "   landseg, luname, paramname, paramval, model_scen, param_row ) ";
      $listobject->querystring .= "select $scenarioid, $subshedid, '$landseg', '$luname', ";
      $listobject->querystring .= "   '$thisparam', $paramcol, '$scname', '$infoid' ";
      $listobject->querystring .= "from $tablename";

      #print("$listobject->querystring \n");

      $listobject->performQuery();
/*
      if (strlen($outfile) > 0) {
         $listobject->querystring = "select $scenarioid, $subshedid, '$landseg', '$luname', ";
         $listobject->querystring .= "   '$thisparam', $paramcol, '$scname', '$infoid' ";
         $listobject->querystring .= "from $tablename";
         $listobject->performQuery();
         $data = $listobject->queryrecords;
         putDelimitedFile("$outfile",$data,",",0,'unix');
      }

*/
   }



?>