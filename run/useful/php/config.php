<?php

error_reporting(1);

$connstring = "host=208.255.155.69 dbname=satest user=postgres password=314159";
$dbconn = pg_connect($connstring, PGSQL_CONNECT_FORCE_NEW);

$scriptname = $_SERVER['PHP_SELF'];

$httppath = '/work/htdocs';

$libpath = "./";
$basedir = "./";
$indir = "./";

# database and file libraries
include("$libpath/psql_functions.php");
include("$libpath/lib.small.php");
include("$libpath/HSPFFunctions.php");
include("$basedir/adminsetup.php");
include("$basedir/hspf.defaults.php");

# query helper
$listobject = new pgsql_QueryObject;
$listobject->dbconn = $dbconn;
$listobject->adminsetuparray = $adminsetuparray;

# create new uci object, this object brings access to many
# functions, such as the abilityu to parse a UCI and write a UCI
$uciobject = new HSPF_UCIobject;
$uciobject->ucidir = $ucidir;
$uciobject->listobject = $listobject;
$uciobject->ucitables = $ucitables;

# Timer
$timer = new timerObject;


#$uciobject = new HSPF_UCIobject;
#$uciobject->ucidir = $ucidir;
#$uciobject->listobject = $listobject;
#$uciobject->ucitables = $ucitables;


?>
