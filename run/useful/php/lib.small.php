<?php


/**************************************************/
/*********         lib.small.php         **********/
/**************************************************/



/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/*********        db functions           **********/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
function listToSQL(&$item, $key) {
   if ( (strlen($item) == 0) or (is_null($item)) ) {
      $item = "''";
   } elseif (!is_numeric(chop($item))) {
      $item = ltrim(rtrim($item));
      $item = "'$item'";
      $ch = ord(substr($item,0,1));
      $l1 = strlen($item);
      $l2 = strlen(ltrim($item));
      $isblank = strstr($item,' ');
      #print("$l1, $l2, $isblank : $ch \n<br>");
      if ( ($isblank) or (strlen($item) == 2) ) {$item = "NULL";}
      #if ( $isblank ) {$item = "NULL";}
   }
}

function parseCSVdb($dbobj,$infilename,$tablename,$tabledef,$debug) {

   $inf = fopen($infilename,'r');
   $sectiondata = array();
   $maxlinewidth = 1000;

   $dbobj->querystring = $tabledef;
   if ($debug) {
      print("$dbobj->querystring<br>\n");
   }
   $dbobj->performquery();

   $headerline = fgets($inf,$maxlinewidth);

   while ($inline = fgets($inf,$maxlinewidth)) {
      $thisline = split(",",$inline);
      array_walk($thisline,'listToSQL');
      $linevals = implode(",",$thisline);
      $dbobj->querystring = "insert into $tablename ($headerline) values ($linevals)";
      if ($debug) {
         print("$dbobj->querystring<br>\n");
      }
      $dbobj->performquery();
   }

   fclose($inf);

}

function parseCSVToTableDef($tablename, $maxstrlen, $inlines, $sep, $temp) {

   # expects an array with at least 2 entries, as delimited lines, with collumn names
   # and a row of sample data.
   $header = $inlines[0];
   $dataline = $inlines[1];
   $labels = split("$sep", $header);
   $data = split("$sep", $dataline);

   if ($temp) {
      $ttext = 'temp';
   } else {
      $ttext = '';
   }

   $cstr = "create $ttext table $tablename ( ";
   $vdel = '';

   for ($i = 0; $i < count($data); $i++) {
      $td = chop($data[$i]);
      $tc = $labels[$i];
      if (is_numeric($td)) {
         $vtype = 'float8';
      } else {
         if (strlen($td) > 255) {
            $strlen = $maxstrlen;
         } else {
            $strlen = 255;
         }
         $vtype = "varchar($strlen)";
      }

      $cstr .= "$vdel $tc $vtype";
      $vdel = ',';
   }

   $cstr .= ")";

   return $cstr;

}

function createDBFromCSV($dbobj, $infilename,$tablename, $maxstr, $istemp, $debug) {

   $inf = fopen($infilename,'r');
   $sectiondata = array();
   $maxlinewidth = 1000;

   $headerline = fgets($inf,$maxlinewidth);

   $testdata = array();
   array_push($testdata, $headerline);
   $i = 0;

   while ($inline = fgets($inf,$maxlinewidth)) {
      if ($i == 0) {
         array_push($testdata, $inline);
         $tabcreate = parseCSVToTableDef($tablename, $maxstr, $testdata, ",", $istemp);
         $dbobj->querystring = $tabcreate;
         if ($debug) {
            print("$dbobj->querystring<br>\n");
         }
         $dbobj->performQuery();
      }

      $thisline = split(",",$inline);
      array_walk($thisline,'listToSQL');
      $linevals = implode(",",$thisline);
      $dbobj->querystring = "insert into $tablename ($headerline) values ($linevals)";
      if ($debug) {
         print("$dbobj->querystring<br>\n");
      }
      $dbobj->performquery();

      $i++;

   }

   fclose($inf);

   return array($headerline);

}


function doGenericCrossTab ($listobject, $basetable, $groupcols, $crosscol, $valcol, $ordercols = 1) {

   # returns a temp table aggregated via a cross-tab formula
   # from the basetable
   # takes a single column to create a cross value of its values


   $listobject->querystring = "select $crosscol from $basetable group by $crosscol";
   if ($ordercols) {
      $listobject->querystring .= " order by $crosscol";
   }
   $listobject->performQuery();
   #print("$listobject->querystring<br>");

   $crosscols = $listobject->queryrecords;

   $groupar = split(",", str_replace(" ", '', $groupcols));

   $cdel = '';
   $conj = '';
   $conja = '';

   $cnames = '';
   $fromtabs = '';
   $condclause = '';
   $firstalias = '';
   $gcols = '';
   $jointext = '';

   #print_r($crosscols);

   foreach ($crosscols as $thiscol) {

      $colname = $thiscol[$crosscol];

    #  print("$colname <br>");
      $thiscolfix = ereg_replace("[^a-zA-Z0-9_]", '', $colname);

      $cnames .= "$cdel \"$thiscolfix\".valcol as \"$thiscolfix\"";

      # $fromtabs .= "$cdel ( select $groupcols, sum($valcol) as valcol from $basetable where $crosscol = '$colname' group by $groupcols) as \"$thiscolfix\" ";
      $fromtabs .= " $jointext ( select $groupcols, sum($valcol) as valcol from $basetable where $crosscol = '$colname' group by $groupcols) as \"$thiscolfix\" ";
      reset($groupar);

      if ($firstalias <> '') {

         $condclause = " on ( ";
         $conj = '';
         foreach ($groupar as $thisgcol) {
           # $condclause .= "$conj \"$firstalias\".$thisgcol = \"$thiscolfix\".$thisgcol ";
            $condclause .= " $conj \"$firstalias\".$thisgcol = \"$thiscolfix\".$thisgcol ";
            $conj = 'and';
         }
         $condclause .= " ) ";
         $fromtabs .= $condclause;

      } else {
         foreach ($groupar as $thisgcol) {
            $gcols .= "$conja \"$thiscolfix\".$thisgcol ";
            $conja = ',';
         }
         # set the column firstalias to the first able alias name
         $firstalias = $thiscolfix;
         $jointext = ' left outer join ';
      }

      # $firstalias = $thiscolfix;
      $cdel = ',';
   }

   # return "select $gcols, $cnames from $fromtabs where $condclause ";
   return "select $gcols, $cnames from $fromtabs ";

}

/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/*********       form functions          **********/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
function fileMultiSelectedForm($varname,$dirpath,$fileext,$height,$selectedfiles) {

   print("<select name='$varname");
   print("[]' size='$height' multiple>\n");
   # cd-rom file
   $selarray = split(',',$selectedfiles);
   $handle=opendir($dirpath);
   $thisdir = array();
   while (false!==($file = readdir($handle))) {
      array_push($thisdir, $file);
   }

   closedir($handle);
   sort($thisdir);

   foreach ($thisdir as $file) {
      if ( ($file != "." && $file != "..") and (substr($file,(strlen($file) - strlen($fileext)),strlen($fileext)) == $fileext) ) {
         if ( in_array($file, $selarray) ) {
            $seltext = " SELECTED";
         } else {
            $seltext = "";
         }
         print ("<option value='$file'$seltext>$file</option>\n");
      }
   }
   print("</select>\n");
}

function showTextField($fieldname,$fieldvalue) {
   print("<input type=text name=$fieldname value='$fieldvalue'>");
}

function showWidthTextField($fieldname,$fieldvalue,$fieldwidth) {
   print("<input type=text name=$fieldname value='$fieldvalue' size=$fieldwidth>");
}

function showHiddenField($fieldname,$fieldvalue) {
   print("<input type=hidden name=$fieldname value='$fieldvalue'>");
}

function showCheckBox($fieldname,$fieldvalue, $checked) {
   print("<input type=checkbox name=$fieldname value='$fieldvalue' $checked>");
}

function showSubmitButton($fieldname,$fieldvalue) {
   print("<input type=submit name=$fieldname value='$fieldvalue'>");
}

function showTFList($fieldname,$fieldvalue) {
   $tsel = '';
   $fsel = '';
   if ( ($fieldvalue == 'false') or ($fieldvalue == 'f') or (!$fieldvalue) ) {
      $fsel = ' selected';
   } else {
      $tsel = ' selected';
   }
   $selhtml = "<select name='$fieldname'>\n";
   $selhtml .= "<option value='true'$tsel>True</option>\n";
   $selhtml .= "<option value='false'$fsel>False</option>\n";
   $selhtml .= "</select>\n";
   print("$selhtml");
}

function showTFListType($fieldname,$fieldvalue,$type) {
   $tsel = '';
   $fsel = '';
   switch ($type) {
      case 1:
      # boolean
         $tval = 1;
         $fval = 0;
      break;

      case 2:
      # tf text
         $tval = 'true';
         $fval = 'false';
      break;

      default:
         $tval = 'true';
         $fval = 'false';
      break;

   }

   if ( ($fieldvalue == $fval) or (!$fieldvalue) ) {
      $fsel = ' selected';
   } else {
      $tsel = ' selected';
   }
   #print("$fieldvalue");
   $selhtml = "<select name='$fieldname'>\n";
   $selhtml .= "<option value='$tval'$tsel>True</option>\n";
   $selhtml .= "<option value='$fval'$fsel>False</option>\n";
   $selhtml .= "</select>\n";
   print("$selhtml");
}



/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************  File Functions   *****************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/

function putDelimitedFile($filename,$thisarray,$thisdelim,$overwrite,$platform) {

   # if $overwrite is false, it will append
   if ($overwrite) {
      $options = 'w';
   } else {
      $options = 'a';
   }

   switch ($platform) {

      case 'unix':
      $endline = "\n";
      break;

      case 'dos':
      $endline = "\r";
      break;

      default:
      $endline = "\r";
      break;
   }

   $fp = fopen($filename,"$options");

   foreach($thisarray as $thisline) {

      $deline = join($thisdelim,$thisline);

      fwrite($fp,"$deline$endline");
   }

   fclose($fp);
}


/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************  Misc Functions   *****************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
/**************************************************/
########################################################################
##                                TimerObject                         ##
########################################################################

class timerObject {
   # created for php 4.x and lower, some functions would be simplified
   # in a php 5.x system
   # creates a basic stopwatch
   var $timestart = 0;
   var $timeend = 0;
   var $timesplit = 0;

   function microtime_float()
   {
      list($usec, $sec) = explode(" ", microtime());
      return ((float)$usec + (float)$sec);
   }

   function startsplit()
   # starts, or takes a split
   {
      $this->timeend = $this->microtime_float();
      $this->timesplit = $this->timeend - $this->timestart;
      $this->timestart = $this->microtime_float();
      $split = 0;
      if ($this->timesplit > 0) {
         $split = $this->timesplit;
      }
      return $split;
   }

} /* end timerObject */



?>