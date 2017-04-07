<?php

  /* functions for accessing postgres database widgets */

function lookupDetail($dbconn,$listtable,$listpkcol,$listcolumns,$selectedcol,$sortcol) {

   $getlistsql = "select $listcolumns from $listtable where $listpkcol = '$selectedcol'";
   if ($sortcol <> "") { $getlistsql .= " order by ".$sortcol; }
   #debug
   #print("$getlistsql<br>");
   $returnvals = array();
   $getlistquery = pg_exec($dbconn,$getlistsql);
   for ($i = 0;$i < pg_numrows($getlistquery);$i++) {
      $getlistrow = pg_fetch_array($getlistquery,$i,PGSQL_ASSOC);
      array_push($returnvals,$getlistrow);
   }
   return $returnvals;
}


####################################################################################
##                                pgsql_QueryObject                               ##
####################################################################################

class pgsql_QueryObject {
   var $queryrecords; /* array containing records returned from query */
   var $tablename;
   var $querystring = "";
   var $whereclause;
   var $dbconn;
   var $maxrecords = -1;
   var $result;
   var $adminsetup;
   var $showlabels = 1;
   var $viewcolumns = '';
   var $pk;
   var $numrecs = 0;
   var $debug = 0;
   var $debugadvanced = 0;
   var $timestart = 0;
   var $timeend = 0;
   var $timesplit = 0;
   var $odbc = 0;
   var $editview = 0;
   var $affectedrows = 0;
   /* debug levels (new, not fully supported)
      1 - show all
      2 - show SQL only
      3 - print misc. debug
      4 - show queries if an error occurs
   */
   var $lastoid = -1;

   var $adminsetuparray = array(
      "test"=>array(
          "table info"=>array("pk"=>"muid", "sortcol"=>""),
          "column info"=>array(
              "testid"=>array("type"=>1,"params"=>"","label"=>"Test ID","visible"=>1)
              )
          )
       );

   function microtime_float()
   {
      list($usec, $sec) = explode(" ", microtime());
      return ((float)$usec + (float)$sec);
   }

   function sw_startsplit()
   {
      $this->timeend = $this->microtime_float();
      $this->timesplit = $this->timeend - $this->timestart;
      $this->timestart = $this->microtime_float();
   }

   function performQuery() {

      if ($this->debug) {
         print("$this->querystring, <br>connection: $this->dbconn <br>");
      }

      $this->sw_startsplit();

   if ($this->odbc) {
      if ( ($this->querystring <> "") && (isset($this->dbconn)) ) {

         $this->queryrecords = array();
         $this->result = odbc_exec($this->dbconn,$this->querystring);
         #$this->result = odbc_exec($this->dbconn,"select * from species");
         switch ($this->debug) {

            case 2:

            break;

            case 4:

            break;

            default:
               print("ODBC connection $this->dbconn - $this->result<br>");
               $nr =  odbc_num_rows($this->result);
               print("Returned $nr records.<br>");
            break;
         }
         $i = 1;
#         while ($thisrow =  odbc_fetch_row($this->result)) {
         while (odbc_fetch_into($this->result, $thisrow)) {
            array_push($this->queryrecords,$thisrow);
            $i++;
         }

      }
   } else {

      if ( ($this->querystring <> "") && (isset($this->dbconn)) ) {
         $this->result = pg_exec($this->dbconn,$this->querystring);
         $this->queryrecords = array();

         # only store a certain number of records, if not, simply store the result and exit
         if ( (pg_numrows($this->result) <= $this->maxrecords) or ($this->maxrecords == -1) ) {
            for ($i = 0;$i < pg_numrows($this->result);$i++) {
               array_push($this->queryrecords,pg_fetch_array($this->result,$i,PGSQL_ASSOC));
            }
         } else {
            print("Max records exceeded in query, result stored.");
         }

         $this->lastoid = pg_getlastoid($this->result);
         $this->affectedrows = pg_affected_rows($this->result);
      }
      if (pg_result_status($this->result) > 4) {
         if ($this->debug == 4) {
            $etext = pg_last_error($this->result);
            print("$etext : <br>$this->querystring <br>");
         }
      }

    }

      $this->numrecs = count($this->queryrecords);

      $this->sw_startsplit();

      switch ($this->debugadvanced) {

         case 0:

         break;

         case 1:
         # show time only
         print(" ( Split Time: $this->timesplit ) ");
         break;

         case 2:
         # show time and query
         print("<br>$this->querystring <br>");
         print(" ( Split Time: $this->timesplit ) ");
         break;

      }


   } /* end function performQuery() */

  function tableExists($thistable) {
     if ($this->debug) {
        print("Searching for table: $thistable<br>");
     }

     $thismeta = false;

     $thismeta = @pg_meta_data($this->dbconn, $thistable);
     $result = true;
     if (!$thismeta) {
        $result = false;
     }

     return $result;
  } /* end function tableExists($thistable) */

  function valueExists($thistable,$thiscolumn,$thisvalue) {
     if ($this->debug) {
        print("Searching for entry: $thisvalue in $thistable($thiscolumn)<br>");
     }

     $this->querystring = "select count(*) from $thistable where $thiscolumn = '$thisvalue'";
     $this->performQuery();

     $result = $this->getRecordValue(1,'count');

     return $result;
  } /* end function valueExists($thistable) */

  function columnExists($thistable,$thiscolumn) {
     if ($this->debug) {
        print("Searching for table: $thistable<br>");
     }

     $thismeta = pg_meta_data($this->dbconn, $thistable);
     $colnames = array_keys($thismeta);
     $result = in_array($thiscolumn,$colnames);

     return $result;
  } /* end function columnExists($thistable,$thiscolumn) */

  function getRecordValue($recnumber, $colname) {
     if ( count($this->queryrecords) >= ($recnumber - 1)) {
        $thisrec = $this->queryrecords[($recnumber -1)];
        $thisval = $thisrec[$colname];
     } else {
        $thisval = "Error: Bad record ID";
     }
     return $thisval;
  }


  function getInsertedRecord($tablename, $colname) {

     $thisval = -1;
     if ($this->lastoid >= 0) {
        $myresult = pg_exec($this->dbconn,"select $colname from $tablename where OID = $this->lastoid");
        $thisrec = pg_fetch_array($myresult,0,PGSQL_ASSOC);
        $thisval = $thisrec["$colname"];
     }
     return $thisval;
  }

   function showDetail() {
      if (count($this->queryrecords) > 0) {
         foreach ($this->queryrecords as $thisrec) {
            $this->detailView($thisrec);
         }
      }
   } /* end showDetail() */

   function detailView($thisrec) {
      if (!isset($this->adminview)) {
         $this->getAdminSetupInfo($this->tablename,$this->viewcolumns);
         if ($this->debug) {
            print("Table: $this->tablename, Cols: $this->viewcolumns <br>");
         }
      }
      $formatinfo = $this->adminsetup;

      if ($formatinfo == "raw") {
         #print("Unformatted output");
         $formatinfo = array();
         $colkeys = array_keys($thisrec);
         $formatinfo["table info"] = array("pk"=>'');
         foreach ($colkeys as $thiskey) {
            $thisformat = array("type"=>1,"label"=>$thiskey,"visible"=>1);
            $formatinfo["column info"][$thiskey] = $thisformat;
         }
      }

      # get the value for this records pk, note: pk records do NOT get displayed
      $tableinfo = $formatinfo["table info"];
      $pkcol = $tableinfo["pk"];

      $pkvalue = $thisrec[$pkcol];
      $rowdesc = $formatinfo["column info"];
      #debug
      if ($this->debug) {
         print("Table: $this->tablename, pk column: $pkcol = $pkvalue<br>");
      }

      #
      # created above in favour of:

      if (strlen($this->viewcolumns) > 0) {
         $colkeys = array_keys($rowdesc);
      } else {
         $colkeys = array_keys($thisrec);
      }

      foreach ($colkeys as $colname) {
         # do the admin setup stuff
         $thisdesc = $rowdesc[$colname];
         $type = $thisdesc["type"];
         $params = $thisdesc["params"];
         $label = $thisdesc["label"];

         $value = $thisrec[$colname];

         if ($this->debug) {
            print("Handling column $colname, type: $type<br>");
         }

         $visible = $thisdesc["visible"];
         if ( ($this->showlabels) && ($visible) ) { print("<b>$label:</b>"); }
         if ( ($this->showlabels) && ($visible) ) {
            $this->printColumnValue($type,$params,$colname,$thisrec,$pkcol,$pkvalue);
         }
         if ($visible) { print("<br>\n"); }

      } # end foreach (column)
   } /* end detailView */

   function showList() {

      $numrecs = count($this->queryrecords);
      if ( ($this->debug == 1) or ($this->debug == 3) ) {

         print("number of records returned for $this->tablename: $numrecs<br>");
         #print_r($this->adminsetup);

      }

      if (!isset($this->adminview)) {
         $this->getAdminSetupInfo($this->tablename,$this->viewcolumns);
      }

      if ( ($this->debug == 1) or ($this->debug == 3) ) {
         print_r($this->adminsetup);
      }

      if (count($this->queryrecords) > 0) {

         $formatinfo = $this->adminsetup;

         if ($formatinfo == "raw") {
            $thisrec = $this->queryrecords[0];
            $formatinfo = array();
            $colkeys = array_keys($thisrec);
            $formatinfo["table info"] = array("pk"=>'');
            foreach ($colkeys as $thiskey) {
               $thisformat = array("type"=>1,"label"=>"$thiskey","visible"=>1);
               $formatinfo["column info"][$thiskey] = $thisformat;
               #print("$thiskey ");
            }
         }

         $rowdesc = $formatinfo["column info"];
         $tabledesc = $formatinfo["table info"];

         # commented out to force only showing columns that have been queried, in order of query
         if ($tabledesc['showascols']) {
            $rkeys = array_keys($rowdesc);
         } else {
            # added next row
            $samplerow = $this->queryrecords[0];
            #modified next row
            $rkeys = array_keys($samplerow);
         }

         $pk = $formatinfo["table info"]["pk"];

         print("\n<table>");

         if ($this->showlabels) {
            print("\n<tr>\n");
            foreach ($rkeys as $thiskey) {
               $label = $rowdesc[$thiskey]["label"];
               $visible = $rowdesc[$thiskey]["visible"];
               if (is_array($rowdesc[$thiskey])) {
                  $fkeys = array_keys($rowdesc[$thiskey]);
               } else {
                  $fkeys = array();
               }

               $halign = 'left';
               if (in_array('halign', $fkeys)) {
                  $halign = $rowdesc[$thiskey]['halign'];
               }
               if ( ($visible) ) { print("<td valign=bottom align=$halign><b>$label</b></td>");}
            }
            print("\n</tr>\n");
         }

         foreach ($this->queryrecords as $thisrec) {

            print("<tr>\n");
            $this->listView($thisrec);
            print("\n</tr>");
         }

         print("\n</table>");
      }
   } /* end function showList() */

   function listView($thisrec) {

      if (!isset($this->adminview)) {
         $this->getAdminSetupInfo($this->tablename,$this->viewcolumns);
      }
      $formatinfo = $this->adminsetup;

      if ($formatinfo == "raw") {
         $formatinfo = array();
         $colkeys = array_keys($thisrec);
         $formatinfo["table info"] = array("pk"=>'');
         foreach ($colkeys as $thiskey) {
            $thisformat = array("type"=>1,"label"=>$thiskey,"visible"=>1);
            $formatinfo["column info"][$thiskey] = $thisformat;
         }
      }

      # get the value for this records pk, note: pk records do NOT get displayed
      $tableinfo = $formatinfo["table info"];
      $pkcol = $tableinfo["pk"];

      $pkvalue = $thisrec["$pkcol"];
      $rowdesc = $formatinfo["column info"];
      #debug
      if ( ($this->debug == 1) or ($this->debug == 3)) {
         print("Formatted row: pk = $pkcol => $pkvalue, visible = $visible, rowdesc = $rowdesc<br>");
      }

      if ( (strlen($this->viewcolumns) > 0) or ($tableinfo['showascols']) ) {
         $colkeys = array_keys($rowdesc);
      } else {
         $colkeys = array_keys($thisrec);
      }

      $rowout = "";
      foreach ($colkeys as $colname) {
         # do the admin setup stuff
         $thisdesc = $rowdesc[$colname];
         $type = $thisdesc["type"];
         $params = $thisdesc["params"];
         $label = $thisdesc["label"];
         $visible = $thisdesc['visible'];
         if (is_array($thisdesc)) {
            $fkeys = array_keys($thisdesc);
         } else {
            $fkeys = array();
         }

         $halign = 'left';
         if (in_array('halign', $fkeys)) {
            $halign = $thisdesc['halign'];
         }
         $valign = 'top';
         if (in_array('valign', $fkeys)) {
            $valign = $thisdesc['valign'];
         }
         if ($formatinfo == "raw") {
            $visible = 1;
         }

         $value = $thisrec[$colname];

         if ( ($this->debug == 1) or ($this->debug == 4)) {
            print("Handling column $colname, type: $type<br>");
         }
         if ($visible) {
            print("<td valign=$valign align=$halign>\n");
            $this->printColumnValue($type,$params,$colname,$thisrec,$pkcol,$pkvalue);
            print("\n</td>\n");
         }

      } # end foreach (column)

   } /* end listView */


   function printColumnValue($type,$params,$cname,$thisrow,$pkcol,$pkvalue) {

      $value = $thisrow[$cname];
      #$pkvalue = $thisrow["$this->pk"];
      #$pkcol = $this->pk;

      # for columns which don;t appear in the adminsetup table
      if (!($type > 0)) { $type = 1;}

      #print("PK: $pkvalue<br>");

      switch ($type) {

      case 0:
      # pk column, do nothing
      break;

      case 3:
      # select list, can show multiple result columns
         list($foreigntable,$keycol,$listcols,$sortcol,$showlabels) = split(":",$params);
         $pkvalue = $value;
         $getlistsql = "select $listcols from $foreigntable where $keycol = '$pkvalue'";
         if ($sortcol <> "") { $getlistsql .= " order by ".$sortcol; }
         if ( ($this->debug > 0) and ($this->debug < 3) ) {
            print("$getlistsql<br>");
         }
         $listobject = new pgsql_QueryObject();
         $listobject->querystring = $getlistsql;
         $listobject->tablename = $foreigntable;
         $listobject->dbconn = $this->dbconn;
         $listobject->showlabels = $showlabels;
         $listobject->performQuery();
         $listobject->showList();
      break;

      case 4:
      # simple select list
         list($foreigntable,$keycol,$listcols,$sortcol,$showlabels) = split(":",$params);
         $pkvalue = $value;
         $getlistsql = "select $listcols from $foreigntable where $keycol = '$value'";
         if ($sortcol <> "") { $getlistsql .= " order by ".$sortcol; }
         #$this->debug = 1;
         if ( ($this->debug > 0) and ($this->debug < 3) ) {
            print("$getlistsql<br>");
         }
         #$this->debug = 0;
         $listobject = new pgsql_QueryObject();
         $listobject->querystring = $getlistsql;
         $listobject->tablename = $foreigntable;
         $listobject->dbconn = $this->dbconn;
         $listobject->showlabels = $showlabels;
         $listobject->performQuery();
         $listobject->showList();
      break;

      case 5:
      # map table
         list($fortable,$maptable,$mapkeycol,$fkeycol,$mapfkeycol,$listcols,$showlabels,$sortcol) = split(":",$params);
         $getlistsql = "select $listcols from $fortable where $maptable.$mapkeycol = '$pkvalue' and $maptable.$mapfkeycol = $fortable.$fkeycol";
         if ($sortcol <> "") { $getlistsql .= " order by ".$sortcol; }
         if ( ($this->debug > 0) and ($this->debug <= 3) ) {
            print("$getlistsql<br>");
            print_r($this->adminsetup);
         }
         #$this->debug = 1;
         $listobject = new pgsql_QueryObject();
         $listobject->querystring = $getlistsql;
         $listobject->debug = $this->debug;
         $listobject->dbconn = $this->dbconn;
         $listobject->showlabels = $showlabels;
         $listobject->performQuery();
         $listobject->tablename = $fortable;
         $listobject->adminsetup = $this->adminsetup;
         $listobject->adminsetuparray = $this->adminsetuparray;
         #$listobject->adminview = 1;
         $listobject->showList();
         #$this->debug = 0;
      break;


      case 6: # n/a number format
         list($nanum,$decimalplaces,$natext) = split(":",$params);
         if ($decimalplaces == '') {
            $decimalplaces = 2;
         }
         if ($value == $nanum) {
            $fvalue = $natext;
         } else {
            $fvalue = number_format($value, $decimalplaces);
         }
         print("$fvalue");
      break;

      case 7: # percent format
         $decimalplaces = $params;
         if ($decimalplaces == '') {
            $decimalplaces = 2;
         }
         $fvalue = number_format(100.0*$value, $decimalplaces);
         print("$fvalue %");
      break;

      case 8: # scientific notation
         list($decimalplaces) = split(":",$params);
         if ($decimalplaces == '') { $decimalplaces = 0; }
         $fvalue = sciFormat($value,$decimalplaces);
         print("$fvalue");
      break;

      case 9: # number format
         $decimalplaces = $params;
         if ($decimalplaces == '') {
            $decimalplaces = 2;
         }
         $fvalue = number_format($value, $decimalplaces);
         print("$fvalue");
      break;

      case 10: # currency format
         list($decimalplaces) = split(":",$params);
         if ($decimalplaces == '') { $decimalplaces = 2; }
         $fvalue = number_format($value, $decimalplaces);
         print("\$$fvalue");
      break;

      case 11: # 2-column map
      # multiple value lookup (aka multi select list)
         list($local1,$local2,$ftable,$foreign1,$foreign2,$listcols,$sortcol,$showlabels) = split(":",$params);
         $l1 = $thisrow[$local1];
         $l2 = $thisrow[$local2];
         $getlistsql = "select $listcols from $ftable where ";
         $getlistsql .= "$foreign1 = '$l1' and $foreign2 = '$l2'";
         if ($sortcol <> "") { $getlistsql .= " order by ".$sortcol; }
         #print("$local1,$local2: $getlistsql<br>");
         $listobject = new pgsql_QueryObject();
         $listobject->querystring = $getlistsql;
         $listobject->tablename = $ftable;
         $listobject->adminsetup = $this->adminsetup;
         $listobject->adminsetuparray = $this->adminsetuparray;
         $listobject->dbconn = $this->dbconn;
         $listobject->showlabels = $showlabels;
         $listobject->performQuery();
         $listobject->showList();
      break;

      case 12: # highlight min or max column value
      # multiple value lookup (aka multi select list)
         list($comptype,$compcol) = split(":",$params);
         $compcols = split(",",$compcol);
         $flagval = "unset";
         $flagcol = "";
         foreach ($compcols as $ccol) {
            $cval = $thisrow[$ccol];
            switch ($comptype) {
            case "min":
               if ( ($cval < $flagval) || ($flagval == "unset") ) {
                  #print("$cval < $flagval <br>");
                  $flagval = $cval;
                  $flagcol = $ccol;
               }
            break;

            case "max":
               if ( ($cval > $flagval) || ($flagval == "unset") ) {
                  $flagval = $cval;
                  $flagcol = $ccol;
               }
            break;
            }
         }
         if ($cname == $flagcol) {
            print("<b>$value</b>");
         } else {
            print("$value");
         }

      break;

      case 13: # print out min or max column value from list of columns in this row
      # multiple value lookup (aka multi select list)
         list($comptype,$nullval,$compcol) = split(":",$params);
         $compcols = split(",",$compcol);
         $flagval = "unset";
         $flagcol = "";
         foreach ($compcols as $ccol) {
            $cval = $thisrow[$ccol];
            switch ($comptype) {
            case "min":
               if ( (($cval < $flagval) || ($flagval == "unset")) && ($cval <> $nullval) ) {
                  #print("$cval < $flagval <br>");
                  $flagval = $cval;
                  $flagcol = $ccol;
               }
            break;

            case "max":
               if ( ($cval > $flagval) || ($flagval == "unset") ) {
                  $flagval = $cval;
                  $flagcol = $ccol;
               }
            break;
            }
         }
         print("$flagval");

      break;

     case 14: # 2-column map
      # multiple value lookup (aka multi select list)
         list($local1,$local2,$ftable,$foreign1,$foreign2,$listcols,$sortcol,$showlabels) = split(":",$params);
         $l1 = $local1;
         $l2 = $thisrow[$local2];
         $getlistsql = "select $listcols from $ftable where ";
         $getlistsql .= "$foreign1 = '$l1' and $foreign2 = '$l2'";
         if ($sortcol <> "") { $getlistsql .= " order by ".$sortcol; }
         #print("$local1,$local2: $getlistsql<br>");
         $listobject = new pgsql_QueryObject();
         $listobject->querystring = $getlistsql;
         $listobject->tablename = $foreigntable;
         $listobject->dbconn = $this->dbconn;
         $listobject->showlabels = $showlabels;
         $listobject->performQuery();
         $listobject->showList();
      break;

      case 15: # edit link
         list($editpage,$extravars,$targetframe,$linktext,$urlextras) = split(":",$params);

         $extralist = split(',',$extravars);
         $extraurl = '';
         $udel = '';
         foreach ($extralist as $extrafield) {
            $thisfield = $thisrow[$extrafield];
            $extraurl .= "$udel$extrafield=$thisfield";
            $udel = '&';
         }

         if (strlen($urlextras) > 0) {
            $extraurl .= "&$urlextras";
         }

         print("<a href='./$editpage?$extraurl&$pkcol=$pkvalue' target='$targetframe'>$linktext </a>");
      break;


      default:
      # text plain, numeric and text types (1,2)
         print("$value");
      break;
      }
   } /* end getColumnValue */

   function formView($thisrec) {

      if (!isset($this->adminview)) {
         $this->getAdminSetupInfo($this->tablename,$this->viewcolumns);
      }
      $formatinfo = $this->adminsetup;

      if ($formatinfo == "raw") {
         $formatinfo = array();
         $colkeys = array_keys($thisrec);
         $formatinfo["table info"] = array("pk"=>'');
         foreach ($colkeys as $thiskey) {
            $thisformat = array("type"=>1,"label"=>$thiskey,"visible"=>1);
            $formatinfo["column info"][$thiskey] = $thisformat;
         }
      }

      # get the value for this records pk, note: pk records do NOT get displayed
      $tableinfo = $formatinfo["table info"];
      $pkcol = $tableinfo["pk"];

      $pkvalue = $thisrec[$pkcol];
      $rowdesc = $formatinfo["column info"];
      #debug
      if (($this->debug == 1)) {
         print("Formatted row: pk = $pkcol => $pkvalue, visible = $visible, rowdesc = $rowdesc<br>");
      }

      if (strlen($this->viewcolumns) > 0) {
         $colkeys = array_keys($rowdesc);
      } else {
         $colkeys = array_keys($thisrec);
      }

      $rowout = "";
      foreach ($colkeys as $colname) {
         # do the admin setup stuff
         $thisdesc = $rowdesc[$colname];
         $type = $thisdesc["type"];
         $params = $thisdesc["params"];
         $label = $thisdesc["label"];
         $readonly = $thisdesc["readonly"];
         $visible = $thisdesc['visible'];
         if ($formatinfo == "raw") {
            $visible = 1;
         }

         $value = $thisrec[$colname];

         if (($this->debug > 0) and ($this->debug < 4)) {
            print("Handling column $colname, type: $type<br>");
         }
         if ($visible) {
            print("<td valign=top>\n");
            if  ( !($readonly) ) {
               $this->printFormField($type,$params,$colname,$thisrec);
            } else {
               $this->printColumnValue($type,$params,$colname,$thisrec,$pkcol,$pkvalue);
            }
            print("\n</td>\n");
         }

      } # end foreach (column)

   } /* end formView */


   function printFormField($type,$params,$colname,$thisrec) {

      $value = $thisrow[$cname];
     $pkvalue = $thisrow[$this->pk];
      $pkcol = $this->pk;

      switch ($type) {

      case 1:
         print("<input type=text name=$colname value='$value'>");
      break;

      default:
         print("<input type=text name=$colname value='$value'>");
      break;

      }

   }

   function getAdminSetupInfo($tablename,$columns) {
      # retrieves admin setup info from table, or from existing array structure
      #      returns an associative array with the following:
      #           "table info" => array("pkcol"=>columname, "sortcol"=>columnname )
      #           "column info" => array(columname1=>array("type"=>displaytype,
      #                                                    "params"=>"param1:param2:param3",
      #                                                    "label"=>string )
      #      returns 0 if table is undefined
      # $dbconn - psql database connection id
      # $tablename - the name of the table to retrieve info for
      # $columns - a comma seperated list of columns to retrieve info for, if this is
      #            blank ALL columns will be retrieved

      $dbconn = $this->dbconn;
      #$exists = include('adminsetup.php'); /* creates array $adminsetuparray */

      #debug
      #print("get adminsetup stuff: $tablename, $this->adminsetuparray<br>");

      $askeys = array_keys($this->adminsetuparray);


      if (($this->debug > 0) and ($this->debug < 2)) {
         print("All adminsetup entries: <br>");
         foreach(array_keys($this->adminsetuparray) as $thiskey) {
            print("&nbsp;&nbsp;&nbsp;$thiskey<br>");
         }
      }

      if (in_array($tablename,$askeys)) {
         $formatinfo = $this->adminsetuparray[$tablename];
         $tableinfo = $formatinfo["table info"];
         $columninfo = $formatinfo["column info"];
         if (($this->debug > 0) and ($this->debug < 4)) {
            print("Columns: $columns <br>");
         }
         if (!($columns == '')) {
            foreach(explode(",",$columns) as $cname) {
               if (($this->debug > 0) and ($this->debug < 4)) {
                  $allcols = implode(",",array_keys($columninfo));
                  print("Name: $cname in $allcols ??<br>");
               }
               if ( in_array($cname, array_keys($columninfo)) ) {
                  #print("$cname located.<br>");
                  $newcolumninfo[$cname] = $columninfo[$cname];
               }
            }
         } else {
            $newcolumninfo = $columninfo;
         }
         $formatinfo["table info"] = $tableinfo;
         $formatinfo["column info"] = $newcolumninfo;
         $this->pk = $tableinfo["pk"];
      } else {
         $formatinfo = "raw";
      }

      if (($this->debug > 0) and ($this->debug < 2)) { print("$formatinfo<br>"); }
      $this->adminsetup = $formatinfo;

   }

    /* end function getAdminSetupInfo($dbconn,$tablename,$columns) */

} /* end pgsql_QueryObject */
?>

