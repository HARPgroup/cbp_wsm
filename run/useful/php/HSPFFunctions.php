<?php

# HSPF UCI Object

class HSPF_UCIobject {

   # basic block variables
   var $perlndgeninfo; /* array containing pervious landuse declarations */
   var $implndgeninfo; /* array containing impervious landuse declarations */
   var $rchresgeninfo; /* array containing rchres landuse declarations */
   var $schematic;
   var $files;
   var $plotinfo;
   var $masslink;
   var $externalsrc;
   var $opnsequence;

   # output block variables
   var $filesblock = array();
   var $schematicblock = array();
   var $nqualsblock = array();
   var $qualpropsblock = array();
   var $qualinputblock = array();

   # properties of UCI
   var $openhandles;
   var $reachcount;
   var $hiperlnd;
   var $loperlnd;
   var $hiimplnd;
   var $loimplnd;
   var $hiplotinfo;
   var $loplotinfo;
   var $uciname;
   var $ucifile;
   var $ucidir = './uci';
   var $logdir = './logs';
   var $outdir = './out';
   var $timestep = '01:00';

   # misc operational vars
   var $debug = 0;

   /* ucitables record format/description
      NAMEOFBLOCK = a unique name which indicates the name of this thing, this resembles the name that is found in the UCI, but is not exactly, like it, as it must be unique to serve as an identifier in this array. For example, the block name "GEN-INFO" is used in "PERLND", "IMPLND" and "RCHRES" blocks of a uci, thus is not a unique descriptor. In this array, those 3 gen-info blocks are given the unique names "PGEN-INFO", "IGEN-INFO" and "RGEN-INFO" to distinguish them.
         'tabledef' = an sql statement used to initialize the temporary table holding the contents of this uci block when parsed into memory.
         'tablename' = the name of the table created by the above statement
         'uciformat' = a format string for the php function sscanf for use in the function "parseUCIpart" for reading in the uci. Fairly standard implementation of the old school fortran formatted read.
         'uciprint' = similar to the above, used by the sprintf command, for outputting the block information back to a UCI file format.
         'ordercol' = tthe column to be used for ordering in an output
         'openline' = the block open line, to be used for outputting a new uci block
         'closeline' = the block close line, to be used for outputting a new uci block
         'header' = the block header, for outputting a new uciblock
         'keytable' = the database table (coming from a parsed uci block) to be used to generate a new version of this block. This table would typically be based on the GEN-INFO table for the given major block, i.e. if this were a block having to do with a RCHRES, this table would typically be the 'rchresgeninfo' table.
         'paramcolumns' = the columns to be used in an sql insert statement generating new paramters for his block.
         'paramdefaults' = the default param values for this block, for use in uci creation.

      Below is a sample record, 'PWAT-PARM2' is the 'NAMEOFBLOCK'

      'PWAT-PARM2'=>array(
               'tabledef'=>'create temp table pwatparm2 (segstart integer, segend integer, forest float8,lzsn float8,infilt float8,lsur float8,slsur float8,kvary float8,agwrc float8)',
                'tablename'=>'pwatparm2',
               'uciformat'=>'%5[^\n]%5[^\n]%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d',
               'uciprint'=>'%5s%5s%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d',
               'ordercol'=>'segstart',
               'openline'=>'  PWAT-PARM2',
               'closeline'=>'  END PWAT-PARM2',
               'header'=>'*** < PLS>    FOREST      LZSN    INFILT      LSUR     SLSUR     KVARY     AGWRC',
                'keytable'=>'perlndgeninfo',
                'paramcolumns'=>'forest,lzsn,infilt,lsur,slsur,kvary,agwrc',
                'paramdefaults'=>'1.0,7.0,0.20,200.0,0.05,0.0,0.98'
                ),
      */

   /* these are default values. They may be overridden by setting this parameter in the file which instantiates the uciobject */
   var $ucitables = array(
          'PGEN-INFO'=>array(
                'tabledef'=>'create temp table perlndgeninfo (segstart integer, segend integer,luname varchar(32))',
                'tablename'=>'perlndgeninfo',
                'uciformat'=>'%5[^\n]%5[^\n]%30[^\]'
                ),
          'IGEN-INFO'=>array(
                'tabledef'=>'create temp table implndgeninfo (segstart integer, segend integer,luname varchar(32))',
                'tablename'=>'implndgeninfo',
                'uciformat'=>'%5[^\n]%5[^\n]%30s'
                ),
          'RGEN-INFO'=>array(
                'tabledef'=>'create temp table rchresgeninfo (segstart integer, segend integer,rchname varchar(32), nexits integer, nin integer, nout integer, f1 integer,f2 integer,f3 integer)',
                'tablename'=>'rchresgeninfo',
                'uciformat'=>'%5[^\n]%5[^\n]%20[^\]%10d%10d%5d%5d%5d%5d'
                ),
          'PERLND-ACTIVITY'=>array(
                'tabledef'=>'create temp table pactivity (segstart integer, segend integer, atmp integer, snow integer, pwat integer, sed integer, pst integer, pwg integer, pqal integer, mstl integer, pest integer, nitr integer, phos integer, trac integer)',
                'tablename'=>'pactivity',
                'uciformat'=>'%5[^\n]%5[^\n]%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d',
                'uciprint'=>'%5s%5s%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d',
                'ordercol'=>'segstart',
                'openline'=>'  ACTIVITY',
                'closeline'=>'  END ACTIVITY',
                'header'=>'*** x -  x ATMP SNOW PWAT  SED  PST  PWG PQAL MSTL PEST NITR PHOS TRAC ***'
                ),
          'IMPLND-ACTIVITY'=>array(
                'tabledef'=>'create temp table iactivity (segstart integer, segend integer, atmp integer, snow integer,iwat integer, sld integer, iwg integer, iqal integer)',
                'tablename'=>'iactivity',
                'uciformat'=>'%5[^\n]%5[^\n]%5d%5d%5d%5d%5d%5d',
                'uciprint'=>'%5s%5s%5d%5d%5d%5d%5d%5d',
                'ordercol'=>'segstart',
                'openline'=>'  ACTIVITY',
                'closeline'=>'  END ACTIVITY',
                'header'=>'*** x -  x ATMP SNOW IWAT  SLD  IWG IQAL'
                ),
          'RCHRES-ACTIVITY'=>array(
                'tabledef'=>'create temp table ractivity (segstart integer, segend integer, hyfg integer, adfg integer, cnfg integer, htfg integer, sdfg integer, gqfg integer, oxfg integer, nufg integer, pkfg integer, phfg integer)',
                'tablename'=>'ractivity',
                'uciformat'=>'%5[^\n]%5[^\n]%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d',
                'uciprint'=>'%5s%5s%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d',
                'ordercol'=>'segstart',
                'openline'=>'  ACTIVITY',
                'closeline'=>'  END ACTIVITY',
                'header'=>'*** x -  x HYFG ADFG CNFG HTFG SDFG GQFG OXFG NUFG PKFG PHFG'
                ),
          'PWAT-PARM1'=>array(
                'tabledef'=>'create temp table pwatparm1 (segstart integer, segend integer,csno integer,rtop integer,uzfg integer,vcs integer,vuz integer, vnn integer,vifw integer,virc integer,vle integer,iffc integer, hwt integer, irrg integer)',
                'keytable'=>'perlndgeninfo',
                'paramcolumns'=>'csno,rtop,uzfg,vcs,vuz,vnn,vifw,virc,vle,iffc,hwt, irrg',
                'paramdefaults'=>'0,0,0,0,0,0,0,0,0,0,0,0',
                'tablename'=>'pwatparm1',
                'uciformat'=>'%5[^\n]%5[^\n]%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d',
                'uciprint'=>'%5s%5s%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d'
                ),
          'PWAT-PARM2'=>array(
                'tabledef'=>'create temp table pwatparm2 (segstart integer, segend integer, forest float8,lzsn float8,infilt float8,lsur float8,slsur float8,kvary float8,agwrc float8)',
                'tablename'=>'pwatparm2',
                'keytable'=>'perlndgeninfo',
                'paramcolumns'=>'forest,lzsn,infilt,lsur,slsur,kvary,agwrc',
                'paramdefaults'=>'1.0,7.0,0.20,200.0,0.05,0.0,0.98',
                'uciformat'=>'%5[^\n]%5[^\n]%10f%10f%10f%10f%10f%10f%10f',
                'uciprint'=>'%5s%5s%6.3f%6.3f%6.3f%7.2f%6.3f%6.3f%6.3f',
               'ordercol'=>'segstart',
               'openline'=>'  PWAT-PARM2',
               'closeline'=>'  END PWAT-PARM2',
               'header'=>'*** < PLS>    FOREST      LZSN    INFILT      LSUR     SLSUR     KVARY     AGWRC'
                ),
          'PWAT-PARM3'=>array(
                'tabledef'=>'create temp table pwatparm3 (segstart integer, segend integer, petmax float8, petmin float8, infexp float8, infild float8, deepfr float8,basetp float8,agwetp float8)',
                'tablename'=>'pwatparm3',
                'keytable'=>'perlndgeninfo',
                'paramcolumns'=>'petmax, petmin, infexp, infild, deepfr,basetp,agwetp',
                'paramdefaults'=>'40.0,35.0,2.0,2.0,0.01,0.01,0.0',
               'ordercol'=>'segstart',
               'openline'=>'  PWAT-PARM3',
               'closeline'=>'  END PWAT-PARM3',
               'header'=>'*** < PLS>    PETMAX    PETMIN    INFEXP    INFILD    DEEPFR    BASETP    AGWETP',
                'uciformat'=>'%5[^\n]%5[^\n]%10f%10f%10f%10f%10f%10f%10f',
                'uciprint'=>'%5s%5s%6.3f%6.3f%6.3f%6.3f%6.3f%6.3f%6.3f'
                ),
          'PWAT-PARM4'=>array(
                'tabledef'=>'create temp table pwatparm4 (segstart integer, segend integer, cepsc float8, uzsn float8, nsur float8, intfw float8, irc float8, lzetp float8)',
                'tablename'=>'pwatparm4',
                'keytable'=>'perlndgeninfo',
                'paramcolumns'=>'cepsc, uzsn, nsur, intfw, irc, lzetp',
                'paramdefaults'=>'0.2,1.0,0.10,1.0,0.5,0.5',
               'ordercol'=>'segstart',
               'openline'=>'  PWAT-PARM4',
               'closeline'=>'  END PWAT-PARM4',
               'header'=>'*** <PLS >     CEPSC      UZSN      NSUR     INTFW       IRC     LZETP',
                'uciformat'=>'%5[^\n]%5[^\n]%10f%10f%10f%10f%10f%10f',
                'uciprint'=>'%5s%5s%6.3f%6.3f%6.3f%6.3f%6.3f%6.3f'
                ),
          'HYDR-PARM2'=>array(
                'tabledef'=>'create temp table hydrparm2 (segstart integer, segend integer, FTBW float8,FTBU float8,LEN float8,DELTH float8,STCOR float8,KS float8,DB50 float8)',
                'tablename'=>'hydrparm2',
                'uciformat'=>'%5[^\n]%5[^\n]%5f%5f%10f%10f%10f%10f%10f',
                'uciprint'=>'%5s%5s%5f%5f%10f%10f%10f%10f%10f'
                ),
          'PLOTINFO'=>array(
                'tabledef'=>'create temp table plotinfo (segstart integer, segend integer, fileno integer, npt integer, nms integer, labl integer, pyr integer, pivl integer, type integer)',
                'tablename'=>'plotinfo',
                'uciformat'=>'%5[^\n]%5[^\n]%5d%5d%5d%5d%5d%5d%5d',
                'uciprint'=>'%5s%5s%5d%5d%5d%5d%5d%5d%5d'
                ),
          'GEN-LABELS'=>array(
                'tabledef'=>'create temp table genlabels (segstart integer, segend integer, ptitle varchar(64), punits varchar(10))',
                'tablename'=>'genlabels',
                'uciformat'=>'%5[^\n]%5[^\n]%50[^\]%10[^\]',
                'uciprint'=>'%5s%5s%-50s%10s'
                ),
          'MUTSINFO'=>array(
                'tabledef'=>'create temp table mutsinfo (segstart integer, segend integer, fileno integer, npt integer, nms integer, nli integer, msfg integer)',
                'tablename'=>'mutsinfo',
                'uciformat'=>'%5[^\n]%5[^\n]%5d%5d%5d%5d%5d',
                'uciprint'=>'%5s%5s%5d%5d%5d%5d%5d'
                ),
          'FILES'=>array(
                'tabledef'=>'create temp table files (handle varchar(16), fileno integer, filepath varchar(128))',
                'tablename'=>'files',
                'uciformat'=>'%6[^\n]  %5[^\n]   %64s',
                'uciprint'=>'%-6s  %5d   %-64s'
                ),
          'SCHEMATIC'=>array(
                'tabledef'=>'create temp table schematic (sname varchar(16), ssegno integer, blankval varchar(32),areafactor float,dname varchar(16), dsegno integer, mlno integer)',
                'tablename'=>'schematic',
                'uciformat'=>'%6[^\n]%4[^\n]%18[^\n]%10[^\n]%11s%4d%7d',
                'uciprint'=>'%-6s%4s%18s%10s     %-6s%4d%7d'
                ),
          'OPN SEQUENCE'=>array(
                  'tabledef'=>'create temp table opensequence (sname varchar(16), ssegno integer)',
                 'tablename'=>'opnsequence',
                'uciformat'=>'      %6s     %3d',
                'uciprint'=>'      %-6s     %3d'
                ),
          'MASS-LINK'=>array(
                'tabledef'=>'create temp table masslink (sname varchar(16), ssegno integer)',
                'tablename'=>'opnsequence',
                'uciformat'=>'      %6s     %3d',
                'uciprint'=>'      %-6s     %3d'
                ),
          'PNQUALS'=>array(
                'tabledef'=>'create temp table pnquals (segstart integer, segend integer, nquals integer)',
                'tablename'=>'pnquals',
                'uciformat'=>'%5[^\n]%5[^\n]%5d',
                'uciprint'=>'%5s%5s%5d'
                ),
          'INQUALS'=>array(
                'tabledef'=>'create temp table inquals (segstart integer, segend integer, nquals integer)',
                'tablename'=>'inquals',
                'uciformat'=>'%5[^\n]%5[^\n]%5d',
                'uciprint'=>'%5s%5s%5d'
                ),
          'PQUAL-PROPS'=>array(
                'tabledef'=>'create temp table pqualprops (segstart integer, segend integer, QUALID varchar(20),QTID varchar(10),QSD integer,VPFW integer,VPFS integer,QSO integer,VQO integer,QIFW integer,VIQC integer,QAGW integer,VAQC integer)',
                'tablename'=>'pqualprops',
                'uciformat'=>'%5[^\n]%5[^\n]%15[^\]',
                'uciprint'=>'%5s%5s%15s%5s%5d%5d%5d%5d%5d%5d%5d%5d%5d'
                ),
          'PQUAL-INPUT'=>array(
                'tabledef'=>'create temp table pqualinput (segstart integer, segend integer, SQO float8,POTFW float8,POTFS float8,ACQOP float8,SQOLIM float8,WSQOP float8,IOQC float8,AOQC float8)',
                'tablename'=>'pqualinput',
                'uciformat'=>'%5[^\n]%5[^\n]%8f%8f%8f%8f%8f%8f%8f%8f',
                'uciprint'=>'%5s%5s%5.2f%5.2f%5.2f%5.2f%5.2f%5.2f%5.2f%5.2f'
                ),
          'IQUAL-PROPS'=>array(
                'tabledef'=>'create temp table iqualprops (segstart integer, segend integer,QUALID archar(20),QTID varchar(10),QSD integer,VPFW integer,QSO integer,VQO integer)',
                'tablename'=>'iqualprops',
                'uciformat'=>'%5[^\n]%5[^\n]%15[^\]',
                'uciprint'=>'%5s%5s%15s%5s%5d%5d%5d%5d'
                ),
          'IQUAL-INPUT'=>array(
                'tabledef'=>'create temp table iqualinput (segstart integer, segend integer, SQO float8,POTFW float8,ACQOP float8,SQOLIM float8,WSQOP float8)',
                'tablename'=>'iqualinput',
                'uciformat'=>'%5[^\n]%5[^\n]%8f%8f%8f%8f%8f',
                'uciprint'=>'%5s%5s%5.2f%5.2f%5.2f%5.2f%5.2f'
                ),
          'ADCALC-DATA'=>array(
                'tabledef'=>'create temp table adcalcdata (segstart integer, segend integer, crrat float8, vol float8)',
                'tablename'=>'adcalcdata',
                'uciformat'=>'%5[^\n]%5[^\n]%10f%10f',
                'uciprint'=>'%5s%5s%7.2f%7.2f'
                ),
          'GQ-GENDATA'=>array(
                'tabledef'=>'create temp table gqgendata (segstart integer, segend integer, NGQL integer, TPFG integer,PHFG integer,ROFG integer,CDFG integer,SDFG integer,PYFG integer,LAT float8)',
                'tablename'=>'gqgendata',
                'uciformat'=>'%5[^\n]%5[^\n]%5d%5d%5d%5d%5d%5d%5d%5d',
                'uciprint'=>'%5s%5s%5d%5d%5d%5d%5d%5d%5d%5d'
                ),
          'GQ-AD-FLAGS'=>array(
                'tabledef'=>'create temp table gqadflags (segstart integer, segend integer, f1 integer,c1 integer,f2 integer,c2 integer,f3 integer,c3 integer)',
                'tablename'=>'gqadflags',
                'uciformat'=>'%5[^\n]%5[^\n]%4d%3d%4d%3d%4d%3d',
                'uciprint'=>'%5s%5s%4d%3d%4d%3d%4d%3d'
                ),
          'GQ-QALDATA'=>array(
                'tabledef'=>'create temp table gqqaldata (segstart integer, segend integer, GQID integer,DQAL integer,CONCID integer,CONV integer,QTYID integer)',
                'tablename'=>'gqqaldata',
                'uciformat'=>'%5[^\n]%5[^\n]%20[^\n]%10s%10s%10s',
                'uciprint'=>'%5s%5s%-20s%10s%10s%10s'
                ),
          'GQ-QALFG'=>array(
                'tabledef'=>'create temp table gqqalfg (segstart integer, segend integer, HDRL integer, OXID integer, PHOT integer, VOLT integer, BIOD integer,  GEN integer, SDAS integer)',
                'tablename'=>'gqqalfg',
                'uciformat'=>'%5[^\n]%5[^\n]%5d%5d%5d%5d%5d%5d%5d',
                'uciprint'=>'%5s%5s%5d%5d%5d%5d%5d%5d%5d'
                ),
          'GQ-FLG2'=>array(
                'tabledef'=>'create temp table gqflg2 (segstart integer, segend integer, HDRL integer, OXID integer, PHOT integer, VOLT integer, BIOD integer,  GEN integer, SBMS integer)',
                'tablename'=>'gqflg2',
                'uciformat'=>'%5[^\n]%5[^\n]%5d%5d%5d%5d%5d%5d%5d',
                'uciprint'=>'%5s%5s%5d%5d%5d%5d%5d%5d%5d'
                ),
          'GQ-GENDECAY'=>array(
                'tabledef'=>'create temp table gqgendecay (segstart integer, segend integer, FSTDEC integer, THFST float8)',
                'tablename'=>'gqgendecay',
                'uciformat'=>'%5[^\n]%5[^\n]%10d%10d',
                'uciprint'=>'%5s%5s%7.2f%7.2f'
                ),
          'GQ-VALUES'=>array(
                'tabledef'=>'create temp table gqvalues (segstart integer, segend integer, nquals integer)',
                'tablename'=>'gqvalues',
                'uciformat'=>'%5[^\n]%5[^\n]%5d',
                'uciprint'=>'%5s%5s%5d'
                ),
          'HYDR-INIT'=>array(
                'tabledef'=>'create temp table hydrinit (segstart integer, segend integer, ivol float8, c1 float8, c2 float8, c3 float8, c4 float8, c5 float8, o1 float8, o2 float8, o3 float8, o4 float8, o5 float8)',
                'tablename'=>'hydrinit',
                'uciformat'=>'%5[^\n]%5[^\n]%10f%10f%5f%5f%5f%5f%10f%5f%5f%5f%5f',
                'uciprint'=>'%5s%5s%7.2f%7.2f%2.2f%2.2f%2.2f%2.2f%7.2f%2.2f%2.2f%2.2f%2.2f'
                ),
          'MON-GENERIC'=>array(
                'tabledef'=>'create temp table mongeneric (segstart integer, segend integer, JAN float8,FEB float8,MAR float8,APR float8,MAY float8,JUN float8,JUL float8,AUG float8,SEP float8,OCT float8,NOV float8,DEC float8)',
                'tablename'=>'mongeneric',
                'uciformat'=>'%5[^\n]%5[^\n]%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f',
                'uciprint'=>'%5s%5s%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f'
                ),
          'FTABLES'=>array(
                'tabledef'=>'create temp table ftables (reachid integer, rdepth float8, rarea float8, rvolume float8,q1 float8, q2 float8)',
                'tablename'=>'ftables',
                'uciformat'=>'%10[^\n]%10[^\n]%10[^\n]%10[^\n]%10[^\n]',
                'uciprint'=>'%7.2f%7.2f%7.2f%7.2f'
                ),
          'MON-UZSN'=>array(
                'tabledef'=>'create temp table monuzsn (segstart integer, segend integer, JAN float8,FEB float8,MAR float8,APR float8,MAY float8,JUN float8,JUL float8,AUG float8,SEP float8,OCT float8,NOV float8,DEC float8)',
                'tablename'=>'monuzsn',
                'uciformat'=>'%5[^\n]%5[^\n]%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f',
                'uciprint'=>'%5s%5s%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f'
                ),
          'MON-INTERCEP'=>array(
                'tabledef'=>'create temp table monintercep (segstart integer, segend integer, JAN float8,FEB float8,MAR float8,APR float8,MAY float8,JUN float8,JUL float8,AUG float8,SEP float8,OCT float8,NOV float8,DEC float8)',
                'tablename'=>'monintercep',
                'uciformat'=>'%5[^\n]%5[^\n]%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f',
                'uciprint'=>'%5s%5s%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f'
                ),
          'MON-LZETP'=>array(
                'tabledef'=>'create temp table monlzetp (segstart integer, segend integer, JAN float8,FEB float8,MAR float8,APR float8,MAY float8,JUN float8,JUL float8,AUG float8,SEP float8,OCT float8,NOV float8,DEC float8)',
                'tablename'=>'monlzetp',
                'uciformat'=>'%5[^\n]%5[^\n]%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f',
                'uciprint'=>'%5s%5s%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f'
                ),
          'MON-MANNING'=>array(
                'tabledef'=>'create temp table monmanning (segstart integer, segend integer, JAN float8,FEB float8,MAR float8,APR float8,MAY float8,JUN float8,JUL float8,AUG float8,SEP float8,OCT float8,NOV float8,DEC float8)',
                'tablename'=>'monmanning',
                'uciformat'=>'%5[^\n]%5[^\n]%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f',
                'uciprint'=>'%5s%5s%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f'
                ),
          'PMON-ACCUM'=>array(
                'tabledef'=>'create temp table pmonaccum (segstart integer, segend integer, JAN float8,FEB float8,MAR float8,APR float8,MAY float8,JUN float8,JUL float8,AUG float8,SEP float8,OCT float8,NOV float8,DEC float8)',
                'tablename'=>'pmonaccum',
                'uciformat'=>'%5[^\n]%5[^\n]%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f',
                'uciprint'=>'%5s%5s%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f'
                ),
          'PMON-SQOLIM'=>array(
                'tabledef'=>'create temp table pmonsqolim (segstart integer, segend integer, JAN float8,FEB float8,MAR float8,APR float8,MAY float8,JUN float8,JUL float8,AUG float8,SEP float8,OCT float8,NOV float8,DEC float8)',
                'tablename'=>'pmonsqolim',
                'uciformat'=>'%5[^\n]%5[^\n]%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f',
                'uciprint'=>'%5s%5s%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f'
                ),
          'IMON-ACCUM'=>array(
                'tabledef'=>'create temp table imonaccum (segstart integer, segend integer, JAN float8,FEB float8,MAR float8,APR float8,MAY float8,JUN float8,JUL float8,AUG float8,SEP float8,OCT float8,NOV float8,DEC float8)',
                'tablename'=>'imonaccum',
                'uciformat'=>'%5[^\n]%5[^\n]%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f',
                'uciprint'=>'%5s%5s%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f'
                ),
          'IMON-SQOLIM'=>array(
                'tabledef'=>'create temp table imonsqolim (segstart integer, segend integer, JAN float8,FEB float8,MAR float8,APR float8,MAY float8,JUN float8,JUL float8,AUG float8,SEP float8,OCT float8,NOV float8,DEC float8)',
                'tablename'=>'imonsqolim',
                'uciformat'=>'%5[^\n]%5[^\n]%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f%5f',
                'uciprint'=>'%5s%5s%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f%2.2f'
                )
           );

   function init() {

      /* parse the UCI file, set up the databases for schematic, geninfo, etc. */

      # verify that file exists and is open
      $this->ucifile = "$this->ucidir/$this->uciname";
      if ($this->uciname == '') {
         print("No UCI file defined.<br>");
      } else {

         $inf = fopen($this->ucifile,'r');
         if (!$inf ) {
            print ("Couldn't Open UCI File named: $this->ucifile <br>");
         }
         fclose($inf);
      }

      # verify that DB list object has been set
      if (! $this->listobject ) {
         die ("No DB List object has been set.<br>");
      }
      if (! $this->listobject->dbconn ) {
         die ("No data connection has been set for the DB List object.<br>");
      }

      # parse geninfo
      $this->parsePERLND();
      if ($this->debug) { print(" PERLND parsed <br>"); }
      $this->parseIMPLND();
      if ($this->debug) { print(" IMPLND parsed <br>"); }
      $this->parseSCHEMATIC();
      if ($this->debug) { print(" SCHEMATIC parsed <br>"); }
      $this->parseRCHRES();
      if ($this->debug) { print(" RCHRES parsed <br>"); }
      $this->parsePLOTINFO();
      if ($this->debug) { print(" PLOTINFO parsed <br>"); }
      $this->parseFILES();
      if ($this->debug) { print(" RCHRES parsed <br>"); }
      $this->parseMUTSIN();
      if ($this->debug) { print(" MUTSINFO parsed <br>"); }
      $this->parseGENLABELS();
      if ($this->debug) { print(" GENLABELS parsed <br>"); }
      $this->initReaches();
      if ($this->debug) { print(" Reaches parsed <br>"); }
      $this->initOpenFiles();
      if ($this->debug) { print(" Open File counted <br>"); }
      $this->initHydrInit();
      if ($this->debug) { print(" Hydr Init parsed <br>"); }

   }

   function initAllFromUCI() {
      if (strlen($this->ucifile) == 0) {
         print("No UCI found, exiting<br>");
         return;
      }

      $this->getUCIBlocks();

      $this->listobject->querystring = "select * from uciblocks where subsection in ('GEN-INFO', 'ACTIVITY')";
      $this->listobject->performQuery();
      $geninfoblocks = $this->listobject->queryrecords;

      foreach ($geninfoblocks as $thisblock) {
         $block = $thisblock['section'];
         $subblock = $thisblock['subsection'];
         # extract lead character for retrieval in hspf.defaults array
         $bp = substr($block,0,1);
         $blockname = "$bp$subblock";
         if ($subblock == 'ACTIVITY') {
            $blockname = "$block-$subblock";
         }
         if (in_array($blockname, array_keys($this->ucitables))) {
            $thisinfo = parseFixedWidthUCI($this->ucifile,$block,$subblock,$this->ucitables[$blockname]["uciformat"],$this->debug);
            makeUCITable($this->listobject,"$blockname",$thisinfo,$this->ucitables,$this->debug);
         } else {
            print("$block - $subblock - $blockname not found in ucitables array, section ignored.");
         }
      }

      $this->listobject->querystring = "select * from uciblocks where section in ('PERLND', 'IMPLND', 'RCHRES') and subsection not in ('GEN-INFO', 'ACTIVITY', '')";
      $this->listobject->performQuery();
      if ($this->debug) {
         $qs = $this->listobject->querystring;
         print("$qs<br>");
         $this->listobject->showList();
      }
      $otherblocks = $this->listobject->queryrecords;

      foreach ($otherblocks as $thisblock) {
         $block = $thisblock['section'];
         $subblock = $thisblock['subsection'];
         $subblockname = $subblock;

         print("Parsing Block: $block - $subblock<br>");

         if (!(in_array($subblockname, array_keys($this->ucitables)))) {
            # try other name combinations, maybe it has been set up as a generic
            # block that fits multiple purposes
            switch ($block) {
               case 'PERLND':
                  $subblockname = "P$subblock";
               break;

               case 'IMPLND':
                  $subblockname = "I$subblock";
               break;

               case 'RCHRES':
                  $subblockname = "R$subblock";
               break;
            }
         }
         if (in_array($subblockname, array_keys($this->ucitables))) {
            $thisinfo = parseFixedWidthUCI($this->ucifile,$block,$subblock,$this->ucitables["$subblockname"]["uciformat"],$this->debug);
            makeUCITable($this->listobject,"$subblockname",$thisinfo,$this->ucitables,$this->debug);
         } else {
            print("$block - $subblock not found in ucitables array, section ignored.");
         }
      }

      # parse schematic
      $thisinfo = parseFixedWidthUCI($this->ucifile,'SCHEMATIC','',$this->ucitables['SCHEMATIC']['uciformat'],$this->debug);
      makeUCITable($this->listobject,'SCHEMATIC',$thisinfo,$this->ucitables,$this->debug);

      $this->listobject->querystring = "select max(segstart),min(segstart) from perlndgeninfo";
     $this->listobject->performquery();
     $this->hiperlnd = $this->listobject->getRecordValue(1, 'max');
     $this->loperlnd = $this->listobject->getRecordValue(1, 'min');

      $this->listobject->querystring = "select max(segstart),min(segstart) from implndgeninfo";
     $this->listobject->performquery();
     $this->hiimplnd = $this->listobject->getRecordValue(1, 'max');
     $this->loimplnd = $this->listobject->getRecordValue(1, 'min');

      $this->listobject->querystring = "select max(segstart) as hirch,min(segstart) as lorch,max(segend) as segend from rchresgeninfo";
     $this->listobject->performquery();
     $segend = $this->listobject->getRecordValue(1, 'segend');
     $this->hirchres = $this->listobject->getRecordValue(1, 'hirch');
     $this->lorchres = $this->listobject->getRecordValue(1, 'lorch');
     if ($segend > $this->hirchres) {
        $this->hirchres = $segend;
     }



   } /* end initAllFromUCI */


   function parsePERLND() {
      $this->perlndgeninfo = parseFixedWidthUCI($this->ucifile,'PERLND','GEN-INFO',$this->ucitables["PGEN-INFO"]["uciformat"],$this->debug);
      makeUCITable($this->listobject,'PGEN-INFO',$this->perlndgeninfo,$this->ucitables,$this->debug);

      $this->listobject->querystring = "select max(segstart),min(segstart) from perlndgeninfo";
     $this->listobject->performquery();
     $this->hiperlnd = $this->listobject->getRecordValue(1, 'max');
     $this->loperlnd = $this->listobject->getRecordValue(1, 'min');

     $this->listobject->querystring = "create temp table hspf_lu as select luname from perlndgeninfo group by luname";
     $this->listobject->performquery();

     $perlndinit = parseFixedWidthUCI($this->ucifile,'PERLND', 'PWAT-PARM1',$this->ucitables["PWAT-PARM1"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'PWAT-PARM1',$perlndinit,$this->ucitables,$this->debug);

     $perlndinit = parseFixedWidthUCI($this->ucifile,'PERLND', 'PWAT-PARM2',$this->ucitables["PWAT-PARM2"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'PWAT-PARM2',$perlndinit,$this->ucitables,$this->debug);

     $perlndinit = parseFixedWidthUCI($this->ucifile,'PERLND', 'PWAT-PARM3',$this->ucitables["PWAT-PARM3"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'PWAT-PARM3',$perlndinit,$this->ucitables,$this->debug);

     $perlndinit = parseFixedWidthUCI($this->ucifile,'PERLND', 'PWAT-PARM4',$this->ucitables["PWAT-PARM4"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'PWAT-PARM4',$perlndinit,$this->ucitables,$this->debug);

     $perlndinit = parseFixedWidthUCI($this->ucifile,'PERLND', 'MON-LZETP',$this->ucitables["MON-LZETP"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'MON-LZETP',$perlndinit,$this->ucitables,$this->debug);

     $perlndinit = parseFixedWidthUCI($this->ucifile,'PERLND', 'MON-MANNING',$this->ucitables["MON-MANNING"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'MON-MANNING',$perlndinit,$this->ucitables,$this->debug);

     $perlndinit = parseFixedWidthUCI($this->ucifile,'PERLND', 'MON-UZSN',$this->ucitables["MON-UZSN"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'MON-UZSN',$perlndinit,$this->ucitables,$this->debug);

     $perlndinit = parseFixedWidthUCI($this->ucifile,'PERLND', 'MON-INTERCEP',$this->ucitables["MON-INTERCEP"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'MON-INTERCEP',$perlndinit,$this->ucitables,$this->debug);

     $perlndinit = parseFixedWidthUCI($this->ucifile,'PERLND', 'MON-ACCUM',$this->ucitables["PMON-ACCUM"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'PMON-ACCUM',$perlndinit,$this->ucitables,$this->debug);

     $perlndinit = parseFixedWidthUCI($this->ucifile,'PERLND', 'MON-SQOLIM',$this->ucitables["PMON-SQOLIM"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'PMON-SQOLIM',$perlndinit,$this->ucitables,$this->debug);

     $perlndinit = parseFixedWidthUCI($this->ucifile,'PERLND', 'MON-IFLW-CONC',$this->ucitables["PMON-IOQC"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'PMON-IOQC',$perlndinit,$this->ucitables,$this->debug);


     $perlndinit = parseFixedWidthUCI($this->ucifile,'PERLND', 'QUAL-INPUT',$this->ucitables["PQUAL-INPUT"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'PQUAL-INPUT',$perlndinit,$this->ucitables,$this->debug);

     $perlndinit = parseFixedWidthUCI($this->ucifile,'PERLND', 'ACTIVITY',$this->ucitables["PERLND-ACTIVITY"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'PERLND-ACTIVITY',$perlndinit,$this->ucitables,$this->debug);
   }

   function parseIMPLND() {
      $this->implndgeninfo = parseFixedWidthUCI($this->ucifile,'IMPLND','GEN-INFO',$this->ucitables["IGEN-INFO"]["uciformat"],$this->debug);
      makeUCITable($this->listobject,'IGEN-INFO',$this->implndgeninfo,$this->ucitables,$this->debug);


      $this->listobject->querystring = "select max(segstart),min(segstart) from implndgeninfo";
     $this->listobject->performquery();
     $this->hiimplnd = $this->listobject->getRecordValue(1, 'max');
     $this->loimplnd = $this->listobject->getRecordValue(1, 'min');

     $implndinit = parseFixedWidthUCI($this->ucifile,'IMPLND','MON-ACCUM',$this->ucitables["IMON-ACCUM"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'IMON-ACCUM',$implndinit,$this->ucitables,$this->debug);

     $implndinit = parseFixedWidthUCI($this->ucifile,'IMPLND','MON-SQOLIM',$this->ucitables["IMON-SQOLIM"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'IMON-SQOLIM',$implndinit,$this->ucitables,$this->debug);

     $implndinit = parseFixedWidthUCI($this->ucifile,'IMPLND','QUAL-INPUT',$this->ucitables["IQUAL-INPUT"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'IQUAL-INPUT',$implndinit,$this->ucitables,$this->debug);

     $implndinit = parseFixedWidthUCI($this->ucifile,'IMPLND','ACTIVITY',$this->ucitables["IMPLND-ACTIVITY"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'IMPLND-ACTIVITY',$implndinit,$this->ucitables,$this->debug);

   }

   function parseRCHRES() {
      $rchresgeninfo = parseFixedWidthUCI($this->ucifile,'RCHRES','GEN-INFO',$this->ucitables["RGEN-INFO"]["uciformat"],$this->debug);
      makeUCITable($this->listobject,'RGEN-INFO',$rchresgeninfo,$this->ucitables,$this->debug);

      $this->listobject->querystring = "select max(segstart) as hirch,min(segstart) as lorch,max(segend) as segend from rchresgeninfo";
     $this->listobject->performquery();
     $segend = $this->listobject->getRecordValue(1, 'segend');
     $this->hirchres = $this->listobject->getRecordValue(1, 'hirch');
     $this->lorchres = $this->listobject->getRecordValue(1, 'lorch');
     if ($segend > $this->hirchres) {
        $this->hirchres = $segend;
     }

      $rchresgeninfo = parseFixedWidthUCI($this->ucifile,'RCHRES','ACTIVITY',$this->ucitables["RCHRES-ACTIVITY"]["uciformat"],$this->debug);
      makeUCITable($this->listobject,'RCHRES-ACTIVITY',$rchresgeninfo,$this->ucitables,$this->debug);

   }

   function parseSCHEMATIC() {

      #print("Parsing schematic<br>");
      $this->schematic = parseFixedWidthUCI($this->ucifile,'SCHEMATIC', '',$this->ucitables["SCHEMATIC"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'SCHEMATIC',$this->schematic,$this->ucitables, $this->debug);

     # do landuse summary by RCHRES, PERLND and IMPLND
     $this->listobject->querystring = "create temp table perlnduse as select a.ssegno as segstart,a.areafactor,b.luname,a.dsegno as reachid from schematic as a,perlndgeninfo as b where a.sname = 'PERLND' and a.dname = 'RCHRES' and a.ssegno = b.segstart";
     $this->listobject->performQuery();
     $this->listobject->querystring = "create temp table implnduse as select a.ssegno as segstart,a.areafactor,b.luname,a.dsegno as reachid from schematic as a,implndgeninfo as b where a.sname = 'IMPLND' and a.dname = 'RCHRES' and a.ssegno = b.segstart";
     $this->listobject->performQuery();
     $this->listobject->querystring = "create temp table reachlnduse as select dsegno as reachid,sum(areafactor) from schematic where sname in ('IMPLND','PERLND') and dname = 'RCHRES' group by reachid";
     $this->listobject->performQuery();

     if ($this->debug) {
        print("$land use debugging<br>");
        $this->listobject->querystring = "select * from perlnduse order by segstart";
        $this->listobject->performQuery();
        $this->listobject->showList();
        $this->listobject->querystring = "select * from implnduse order by segstart";
        $this->listobject->performQuery();
        $this->listobject->showList();
        $this->listobject->querystring = "select * from reachlnduse order by reachid";
        $this->listobject->performQuery();
        $this->listobject->showList();
     }
   }


   function initHydrInit() {
      $hydrinit = parseFixedWidthUCI($this->ucifile,'RCHRES', 'HYDR-INIT',$this->ucitables["HYDR-INIT"]["uciformat"], $this->debug);
        makeUCITable($this->listobject,'HYDR-INIT',$hydrinit,$this->ucitables,$this->debug);

     $hydrinit = parseFixedWidthUCI($this->ucifile,'RCHRES', 'HYDR-PARM2',$this->ucitables["HYDR-PARM2"]["uciformat"], $this->debug);
        makeUCITable($this->listobject,'HYDR-PARM2',$hydrinit,$this->ucitables,$this->debug);
   }

   function parseMUTSIN() {
      $mutsinfo = parseFixedWidthUCI($this->ucifile,'MUTSIN', 'MUTSINFO',$this->ucitables["MUTSINFO"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'MUTSINFO',$mutsinfo,$this->ucitables,$this->debug);
   }

   function parseGENLABELS() {
      $mutsinfo = parseFixedWidthUCI($this->ucifile,'PLTGEN', 'GEN-LABELS',$this->ucitables["GEN-LABELS"]["uciformat"], $this->debug);
     makeUCITable($this->listobject,'GEN-LABELS',$mutsinfo,$this->ucitables,$this->debug);
   }


   function parseFILES() {
      $this->files = parseFixedWidthUCI($this->ucifile,'FILES','',$this->ucitables["FILES"]["uciformat"],0);
      makeUCITable($this->listobject,'FILES',$this->files,$this->ucitables,$this->debug);
   }

   function parsePLOTINFO() {
      $this->plotinfo = parseFixedWidthUCI($this->ucifile,'PLTGEN','PLOTINFO', $this->ucitables["PLOTINFO"]["uciformat"],$this->debug);
      makeUCITable($this->listobject,'PLOTINFO',$this->plotinfo,$this->ucitables,$this->debug);

      $this->listobject->querystring = "select max(segstart),min(segstart) from plotinfo";
     $this->listobject->performquery();
     $this->hiplotinfo = $this->listobject->getRecordValue(1, 'max');
     $this->loplotinfo = $this->listobject->getRecordValue(1, 'min');
   }

   function initOpenFiles() {

   # create table holding open file handles
      $pltgenfiles = '';
      $qobj = $this->listobject;
     $this->listobject->querystring = "create temp table openhandles (fileno integer)";
     $this->listobject->performquery();
     for ($i = 1;$i <= 99;$i++) {
        $qobj->querystring = "insert into openhandles values($i)";
        $debout = $qobj->querystring;
        if ($this->debug) { print("$debout <br>"); }
        $qobj->performquery();
     }
     $this->listobject->querystring = "delete from openhandles where fileno in (select fileno from files)";
     $this->listobject->performquery();

   }

   function showQuery() {
      $thisquery = $this->listobject->querystring;
      print("Query: $thisquery <br>");
   }

   function getFileHandle($lowerlimit,$upperlimit) {

      # returns a handle from the pool of open file handles, deletes the handle from pool

      $this->listobject->querystring = "select fileno from openhandles where fileno <= $upperlimit and fileno >= $lowerlimit limit 1";
      if ($this->debug) { $this->showQuery(); }
      $this->listobject->performquery();
      if ($this->listobject->numrecs > 0) {
         $fno = $this->listobject->getRecordValue(1, 'fileno');
      } else {
         $fno = -1;
      }

      # delete this handle from the open handles table
      $this->listobject->querystring = "delete from openhandles where fileno = $fno";
      if ($this->debug) { $this->showQuery(); }
      $this->listobject->performquery();

      return $fno;
   }

   function initReaches() {
      $this->listobject->querystring = "create temp table reaches as select distinct(dsegno) as reachid from schematic where dname = 'RCHRES'";
      $this->listobject->performquery();
      $this->countReaches();
      if ($this->debug) {
         print("$this->listobject->querystring <br>");
         print("$this->reachcount reaches found. <br>");
      }
   }

   function countReaches() {

     $this->listobject->querystring = "select count(*) as reachcount from reaches";
     $this->listobject->performquery();
      $this->reachcount = $this->listobject->getRecordValue(1, 'reachcount');
   }

   function addFile($startno,$endno,$thiswdm,$thisname) {

      $thishandle = $this->getFileHandle($startno,$endno);
      if ($thishandle <> -1) {
         $fileline = sprintf($this->ucitables["FILES"]["uciprint"],$thiswdm, $thishandle,$thisname);
      } else {
         $fileline = "*** COULD NOT ADD FILE $thisname, No open handles";
      }
      array_push($this->filesblock,$fileline);

      $this->listobject->querystring = "insert into files (handle,fileno,filepath) values ('$thiswdm','$thishandle','$thisname')";
      $this->listobject->performQuery();

      return $thishandle;

   }

   function printBlock($thisblock) {
      print("<PRE>");
      foreach ($thisblock as $singleblock) {
         print("$singleblock <br>");
      }
      print("</PRE>");
   } /* end printBlock */


   function initUCIBlockParams($blockname) {

      if ($this->debug) {
         print("Initializing Block: $blockname<br>");
      }

      if ($blockname == 'SCHEMATIC') {
         return;
      }
      $paramcolumns = $this->ucitables["$blockname"]["paramcolumns"];
      $defaultvalues = $this->ucitables["$blockname"]["paramdefaults"];
      $thistable = $this->ucitables["$blockname"]["tablename"];
      $tabledef = $this->ucitables["$blockname"]["tabledef"];

      if ( in_array('keytable',array_keys($this->ucitables["$blockname"])) ) {
         $keytable = $this->ucitables["$blockname"]["keytable"];
      } else {
         $keytable = $thistable;
      }

      $parentblock = $this->ucitables["$blockname"]["parentblock"];
      $subblock = $this->ucitables["$blockname"]["subblock"];


#      /* check to see if table has been created*/
      if ( !($this->listobject->tableExists($thistable)) ) {

         $this->listobject->querystring = $tabledef;
         if ($this->debug) {
            $thisq = $this->listobject->querystring;
            print("$thisq<br>");
         }
         $this->listobject->performquery();

         /* try to read it from UCI */
           #$tableinit = parseFixedWidthUCI($this->ucifile,$parentblock, $subblock,$this->ucitables["$blockname"]["uciformat"], $this->debug);
             #makeUCITable($this->listobject,$blockname,$tableinit,$this->ucitables,$this->debug);
      }


      /* only do this if there are no params loaded for this block
        or if there are less than the total*/

      if ( in_array('paramcolumns',array_keys($this->ucitables["$blockname"])) ) {
         $segdel = ",";
      } else {
         $segdel = "";
      }

      switch ($blockname) {

         case 'FILES':
            if (!($this->listobject->tableexists('openhandles'))) {
               $this->initOpenFiles();
            }
         break;

         case 'PERLND-ACTIVITY':
            $this->listobject->querystring = "insert into $thistable (segstart,segend$segdel$paramcolumns) select min(segstart),max(segstart)$segdel$defaultvalues from $keytable";
            if ($this->debug) {
            $thisq = $this->listobject->querystring;
            print("$thisq<br>");
            }
            $this->listobject->performQuery();
         break;

         case 'IMPLND-ACTIVITY':
            $this->listobject->querystring = "insert into $thistable (segstart,segend$segdel$paramcolumns) select min(segstart),max(segstart)$segdel$defaultvalues from $keytable";
            if ($this->debug) {
            $thisq = $this->listobject->querystring;
            print("$thisq<br>");
            }
            $this->listobject->performQuery();
         break;

         case 'RCHRES-ACTIVITY':
            $this->listobject->querystring = "insert into $thistable (segstart,segend$segdel$paramcolumns) select min(segstart),max(segstart)$segdel$defaultvalues from $keytable";
            if ($this->debug) {
            $thisq = $this->listobject->querystring;
            print("$thisq<br>");
            }
            $this->listobject->performQuery();
         break;

         case 'OPN SEQUENCE':

            $this->listobject->querystring = "create temp table tmp_schem as select *, (sname||ssegno) as sid, (dname||dsegno) as did from schematic";
            if ($this->debug) {
            $thisq = $this->listobject->querystring;
            print("$thisq<br>");
            }

            $this->listobject->performQuery();

            if ($this->debug) {
               $this->listobject->querystring = "select * from tmp_schem";
            $thisq = $this->listobject->querystring;
            print("$thisq<br>");
            $this->listobject->performQuery();
            $this->listobject->showList();
            }

            $streamsleft = 1;

            # stash dangling destination entities in a seperate table
            $this->listobject->querystring = "create temp table tmp_dangle as select dname,dsegno from tmp_schem where did not in (select sid from tmp_schem) group by dname,dsegno";
            if ($this->debug) {
            $thisq = $this->listobject->querystring;
            print("$thisq<br>");
            }
            $this->listobject->performQuery();

            while ($streamsleft > 0) {

               # put in OPN SEQUENCE if they have NO remaining unopened contributors
               # remaining in temp schematic table
               $this->listobject->querystring = "insert into $thistable (sname,ssegno) select sname,ssegno from tmp_schem where sid not in (select did from tmp_schem) group by sname,ssegno";
               if ($this->debug) {
                $thisq = $this->listobject->querystring;
               print("$thisq<br>");
               }
               $this->listobject->performQuery();

               # remove members that have already been activated in OPN SEQUENCE
               # and are NOT a non-source entity such as a plotgen
               $this->listobject->querystring = "delete from tmp_schem where sname = $thistable.sname and ssegno = $thistable.ssegno";
               if ($this->debug) {
                $thisq = $this->listobject->querystring;
                print("$thisq<br>");
               }
               $this->listobject->performQuery();

               # count remaining entries in schematic
               $this->listobject->querystring = "select * from tmp_schem";
               $this->listobject->performQuery();
               if ($this->debug) {
               print("Remaining Entries<br>");
               $this->listobject->showList();
               }

               $streamsleft = $this->listobject->numrecs;

               if ($lastleft == $streamsleft) {
                  print("There is a problem with your model linkages, please check that each perlnd has a subshedid set in the model parameter table.<br>");
                  die;
               }

               $lastleft = $streamsleft;
            }

            # retrieve dangling destination entities, do RCHRES/PERLND/IMPLND,
            # followed by COPY, then PLTGEN
            $this->listobject->querystring = "insert into $thistable (sname,ssegno) select dname,dsegno from tmp_dangle where dname not in ('PLTGEN', 'COPY')";
            if ($this->debug) {
            $thisq = $this->listobject->querystring;
            print("$thisq<br>");
            }
            $this->listobject->performQuery();
            $this->listobject->querystring = "delete from tmp_dangle where dname not in ('PLTGEN', 'COPY')";
            $this->listobject->performQuery();

            $this->listobject->querystring = "insert into $thistable (sname,ssegno) select dname,dsegno from tmp_dangle where dname <> 'PLTGEN'";
            if ($this->debug) {
            $thisq = $this->listobject->querystring;
            print("$thisq<br>");
            }
            $this->listobject->performQuery();
            $this->listobject->querystring = "delete from tmp_dangle where dname <> 'PLTGEN'";
            $this->listobject->performQuery();

            $this->listobject->querystring = "insert into $thistable (sname,ssegno) select dname,dsegno from tmp_dangle where dname = 'PLTGEN'";
            if ($this->debug) {
            $thisq = $this->listobject->querystring;
            print("$thisq<br>");
            }
            $this->listobject->performQuery();

         break;

         default:
            $this->listobject->querystring = "insert into $thistable (segstart,segend$segdel$paramcolumns) select segstart,segend$segdel$defaultvalues from $keytable where segstart not in (select segstart from $thistable) order by segstart";
            if ($this->debug) {
            $thisq = $this->listobject->querystring;
            print("$thisq<br>");
            }
            $this->listobject->performQuery();
         break;
      }

   } /* end initUCIBlockParams */

   function outputUCIpart($thisucipart) {

      if ( in_array('defaultoutput',array_keys($this->ucitables["$thisucipart"])) ) {

         $outtype = $this->ucitables["$thisucipart"]['defaultoutput'];

         if ($outtype == 'grouped') {

            $outarr = $this->outputGroupedUCIpart($thisucipart);
            return $outarr;
         }

      }


      $outarr = array();

      if ( in_array('openline',array_keys($this->ucitables["$thisucipart"])) ) {
         $openline = $this->ucitables["$thisucipart"]['openline'];
         array_push($outarr,$openline);
      }
      if ( in_array('header',array_keys($this->ucitables["$thisucipart"])) ) {
         $header = $this->ucitables["$thisucipart"]['header'];
         if ($thisucipart == 'OPN SEQUENCE') {
            $header .= " $this->timestep";
         }
         array_push($outarr,$header);
      }

      $tablename = $this->ucitables["$thisucipart"]["tablename"];

      switch ($thispart) {

         default:

            $this->listobject->querystring = "select * from $tablename";

            if ( in_array('ordercol',array_keys($this->ucitables["$thisucipart"])) ) {
               $ordercol = $this->ucitables["$thisucipart"]["ordercol"];
               $this->listobject->querystring .= " order by $ordercol";
            }

            if ($this->debug) {
               $thisq = $this->listobject->querystring;
               print("$thisq<br>");
            }


            $this->listobject->performQuery();

            foreach($this->listobject->queryrecords as $thispland) {
               $rid = $thispland[0];
                     #if ($this->debug) { print_r($thispland); }
               $thisoutline = arraySprintf($this->ucitables["$thisucipart"]["uciprint"],$thispland);
               array_push($outarr,$thisoutline);
            }

            if ( in_array('closeline',array_keys($this->ucitables["$thisucipart"])) ) {
               $closeline = $this->ucitables["$thisucipart"]['closeline'];
              array_push($outarr,$closeline);
            }

            if ($this->debug) {

               $this->printBlock($outarr);
            }
         break;
      }

      return $outarr;
  } /* end outputUCIpart */

function outputGroupedUCIpart($thisucipart) {


      $outarr = array();

      if ( in_array('openline',array_keys($this->ucitables["$thisucipart"])) ) {
         $openline = $this->ucitables["$thisucipart"]['openline'];
         array_push($outarr,$openline);
      }
      if ( in_array('header',array_keys($this->ucitables["$thisucipart"])) ) {
         $header = $this->ucitables["$thisucipart"]['header'];
         if ($thisucipart == 'OPN SEQUENCE') {
            $header .= " $this->timestep";
         }
         array_push($outarr,$header);
      }

      $tablename = $this->ucitables["$thisucipart"]["tablename"];
      $columns = $this->ucitables["$thisucipart"]["paramcolumns"];

      $this->listobject->querystring = "select min(segstart) as segstart,max(segstart) as segend,$columns from $tablename group by $columns";
      if ($this->ucitables["$thisucipart"]["ordercol"]) {
         $ordercol = $this->ucitables["$thisucipart"]["ordercol"];
         $listobject->querystring .= "order by $ordercol";
      }

      if ($this->debug) {
         $thisq = $this->listobject->querystring;
         print("$thisq<br>");
      }
      $this->listobject->performQuery();

      if ($this->debug) {
         $this->listobject->showlist();
      }

      foreach($this->listobject->queryrecords as $thispland) {
         $rid = $thispland[0];
         $thisoutline = arraySprintf($this->ucitables["$thisucipart"]["uciprint"],$thispland);
         array_push($outarr,$thisoutline);
      }

      if ( in_array('closeline',array_keys($this->ucitables["$thisucipart"])) ) {
         $closeline = $this->ucitables["$thisucipart"]['closeline'];
        array_push($outarr,$closeline);
      }

      if ($this->debug) {

         $this->printBlock($outarr);
      }

      return $outarr;
  } /* end outputGroupedUCIpart */

  function initImperv() {

     $thissetup = $this->ucitables["IMPERVIOUS"];
     $datalines = $thissetup['impdefs'];
     makeUCITable($this->listobject,'IMPERVIOUS',$datalines,$this->ucitables,$this->debug);
  }

  function addMultiPlotGen($srcname,$srcsegno, $afactor, $mlno, $filehandle, $plotid, $constituent, $nms, $npt, $pivl) {

     if ($filehandle == -1) {
        # create a custom plotgen file
        $filehandle = $this->addFile(30,99,'',"$srcname$srcsegno$constituent.out");
     }


     # get the next plotid
     $this->listobject->querystring = "select max(segstart) from plotinfo";
     $this->listobject->performquery();
     if ($debug) { $this->listobject->showlist();}
     $nextplotid = $this->listobject->getRecordValue(1, 'max');
     $nextplotid = intval($nextplotid);
     $nextplotid++;

     if ( isset($plotid) and ($plotid > 0) ) {
        $nextplotid = $plotid;
     } else {
        $this->listobject->querystring = "insert into plotinfo (segstart, fileno, npt, nms, labl, pyr, pivl, type) values ($nextplotid,$filehandle,$npt,$nms,0,9,$pivl,1)";
        if ($this->debug) {
           $ds = $this->listobject->querystring;
           print("$ds<br>");
        }
        $this->listobject->performquery();
     }

     if ($debug) { print("Next available plot ID: $nextplotid <br>");}

     $this->listobject->querystring = "insert into schematic (sname,ssegno, blankval, areafactor, dname, dsegno, mlno) values ('$srcname', $srcsegno, '', $afactor, 'PLTGEN', $nextplotid, $mlno)";
     if ($this->debug) {
        $ds = $this->listobject->querystring;
        print("$ds<br>");
     }
     $this->listobject->performquery();

     $outarr = array(plotid=>$nextplotid,filehandle=>$filehandle);
     return $outarr;


  }

  function addPlotGen($srcname,$srcsegno,$mlno,$filehandle,$plotid,$constituent) {

     if ($filehandle == -1) {
        # create a custom plotgen file
        $filehandle = $this->addFile(30,99,'',"$srcname$srcsegno$constituent.out");
     }


     # get the next plotid
     $this->listobject->querystring = "select max(segstart) from plotinfo";
     $this->listobject->performquery();
     if ($debug) { $this->listobject->showlist();}
     $nextplotid = $this->listobject->getRecordValue(1, 'max');
     $nextplotid = intval($nextplotid);
     $nextplotid++;

     if ( isset($plotid) and ($plotid > 0) ) {
        $nextplotid = $plotid;
     } else {
        $this->listobject->querystring = "insert into plotinfo (segstart, fileno, npt, nms, labl, pyr, pivl, type) values ($nextplotid,$filehandle,0,1,0,9,24,1)";
        if ($this->debug) {
           $ds = $this->listobject->querystring;
           print("$ds<br>");
        }
        $this->listobject->performquery();
     }

     if ($debug) { print("Next available plot ID: $nextplotid <br>");}

     $this->listobject->querystring = "insert into schematic (sname,ssegno, blankval, areafactor, dname, dsegno, mlno) values ('$srcname', $srcsegno, '', 1.0, 'PLTGEN', $nextplotid, $mlno)";
     if ($this->debug) {
        $ds = $this->listobject->querystring;
        print("$ds<br>");
     }
     $this->listobject->performquery();

     $outarr = array(plotid=>$nextplotid,filehandle=>$filehandle);
     return $outarr;


  }

  function addMutsin($mutname,$destname,$srcsegno,$seriestype,$numcols,$mlno,$filehandle,$mutid) {

     if ($filehandle == -1) {
        # create a custom plotgen file
        $filehandle = $this->addFile(30,99,'',"$mutname");
     }


     # get the next plotid
     $this->listobject->querystring = "select max(segstart) from mutsinfo";
     $this->listobject->performquery();
     if ($debug) { $this->listobject->showlist();}
     $nextplotid = $this->listobject->getRecordValue(1, 'max');
     $nextplotid = intval($nextplotid);
     $nextplotid++;

     switch ($seriestype) {
        case 1:
          # mean
          $npt = 0;
          $nmn = $numcols;
        break;

        case 2:
          # mean
          $npt = $numcols;
          $nmn = 0;
        break;

        default:
          # mean
          $npt = $numcols;
          $nmn = 0;
        break;
     }

     if ( isset($mutid) and ($mutid > 0) ) {
        $nextplotid = $mutid;
     } else {
        $this->listobject->querystring = "insert into mutsinfo (segstart, fileno, npt, nms, nli, msfg) values ($nextplotid,$filehandle,$npt,$nmn,25,3)";
        if ($this->debug) {
           $ds = $this->listobject->querystring;
           print("$ds<br>");
        }
        $this->listobject->performquery();
     }

     if ($debug) { print("Next available plot ID: $nextplotid <br>");}

     $this->listobject->querystring = "insert into schematic (sname,ssegno, blankval, areafactor, dname, dsegno, mlno) values ('MUTSIN', $nextplotid, '', 1.0, '$destname', $srcsegno, $mlno)";
     if ($this->debug) {
        $ds = $this->listobject->querystring;
        print("$ds<br>");
     }
     $this->listobject->performquery();


  }

  function addCopy($srclist,$srcname,$mlno,$addplotgen) {

     if ($addplotgen) {
        # create a custom plotgen file
        addPlotGen($srcname,$srcsegno,$mlno,-1);
     }


     # get the next plotid
     $this->listobject->querystring = "select max(segstart) from copytimeseries";
     $this->listobject->performquery();
     if ($debug) { $this->listobject->showlist();}
     $nextplotid = $this->listobject->getRecordValue(1, 'max');
     $nextplotid = intval($nextplotid);
     $nextplotid++;

     if ($debug) { print("Next available plot ID: $nextplotid <br>");}

     $this->listobject->querystring = "insert into plotinfo (segstart, fileno, npt, nms, labl, pyr, pivl, type) values ($nextplotid,$filehandle,0,1,0,9,24,1)";
     $this->listobject->performquery();

     $this->listobject->querystring = "insert into schematic (sname,ssegno, blankval, areafactor, dname, dsegno, mlno) values ('$srcname', $srcsegno, '', 1.0, 'PLTGEN', $nextplotid, $mlno)";
     $this->listobject->performquery();

  }

  function getUCIBlocks() {
     # opens a standard HSPF .uci file, returns all sections and sub-sections
     # in array format:
     # array(
     #  array("section"=>'PERLND', "subsection"=>''),
     #  array("section"=>'PERLND', "subsection"=>'PWAT-PARM1'),
     #  array("section"=>'PERLND', "subsection"=>'PWAT-PARM2'),

     $inf = fopen($this->ucifile,'r');
     $sectiondata = array();
     $maxlinewidth= 2000;
     $insection = 0;
     $insubsection = 0;

     $currentsection = '';
     $currentsubsection = '';

     $this->listobject->querystring = 'create temp table uciblocks(section varchar(64), subsection varchar(64), subsectionid varchar(64))';
     $this->listobject->performQuery();

     # parse till file ends
     while ( $inline = fgets($inf,$maxlinewidth) ) {

        $k++;

        $inline = ltrim(chop($inline));

        $thisblock = array("section"=>'',"subsection"=>'');
        # test for comment line, if not a comment, process
        if ( !(strstr($inline,'***')) and ($inline <> '') ) {

           $lineidentified = 0;
           while (!$lineidentified) {

              $thisline = preg_split("/[\s,]+/",$inline);

              if ($insection and (($thisline[1] == 'EXT') or ($thisline[1] == 'OPN'))) {
                 $thisline[1] = "$thisline[1] $thisline[2]";
                 array_pop($thisline);
              }

              if (!$insection and (($thisline[0] == 'EXT') or ($thisline[0] == 'OPN'))) {
                 $thisline[0] = "$thisline[0] $thisline[1]";
                 array_pop($thisline);
              }

              if ($insection and ($thisline[0] == 'END') and ($thisline[1] == $currentsection) ) {
                 if ($this->debug) {
                    print("case 1: $inline<br>");
                 }
                 $insection = 0;
                 $currentsection = '';
                 $insubsection = '';
                 $currentsubsection = '';
                 $lineidentified = 1;
                 break;
              }

              # stop if at a block that does NOT contain sub-blocks
              if ( ($currentsection == 'FTABLES') or ($currentsection == 'MASS-LINK') or ($currentsection == 'SCHEMATIC') or ($currentsection == 'EXT SOURCES') or ($currentsection == 'EXT TARGETS') or ($currentsection == 'GLOBAL') or ($currentsection == 'FILES') or ($currentsection == 'OPN SEQUENCE')) {
                 $lineidentified = 1;
                 break;
              }

              if ( $insection and $insubsection ) {
                 if ($this->debug) {
                    print("case 2: $inline<br>");
                 }
                 if ($thisline[0] == "END") {
                    $insubsection = 0;
                    $currentsubsection = '';
                    $lineidentified = 1;
                    break;
                 }
              }

              if ( ($insection) and (!$insubsection) ) {
                 if ($this->debug) {
                    print("case 3: $inline<br>");
                 }
                 if ($thisline[0] == "END") {
                    $insection = 0;
                    $currentsection = '';
                    $lineidentified = 1;
                    break;
                 } else {
                    $insubsection = 1;
                    $currentsubsection = $thisline[0];
                    $thisblock = array('section'=>"$currentsection",    'subsection'=>"$currentsubsection");
                    array_push($sectiondata,$thisblock);
                    $subsectionid = '';
                    if (count($thisline) > 1) {
                       $subsectionid = $thisline[1];
                    }
                    $this->listobject->querystring = "insert into uciblocks (section,subsection,subsectionid) values ('$currentsection','$currentsubsection','$subsectionid')";
                    $this->listobject->performQuery();
                    $lineidentified = 1;
                    break;
                 }
              }

              if ( (!$insection) and (!$insubsection) and ($thisline[0] <> 'RUN') and ($thisline[0] <> 'END')) {
                 if ($this->debug) {
                    print("case 4: $inline<br>");
                 }
                 $currentsection = $thisline[0];
                 $thisblock = array('section'=>"$currentsection", 'subsection'=>'');
                 array_push($sectiondata,$thisblock);
                 $subsectionid = '';
                 if (count($thisline) > 1) {
                    $subsectionid = $thisline[1];
                 }
                 $this->listobject->querystring = "insert into uciblocks (section,subsection,subsectionid) values ('$currentsection','$currentsubsection','$subsectionid')";
                 $this->listobject->performQuery();
                 $insection = 1;
                 $lineidentified = 1;
                 break;
              }


              break;
           }
        }
     }

     if ($this->debug) {
        print("$k lines scanned.\n");
        print_r($sectiondata);
     }
     fclose($inf);
     return $sectiondata;
   } /* end getUCIBlocks */


   function summarizeParameters() {


   $this->listobject->querystring = "select round(min(lzsn)::numeric,3) as lzsn_min,round(max(lzsn)::numeric,3) as lzsn_max,min(infilt) as infilt_min,max(infilt) as infilt_max, round(min(lsur)::numeric,2) as lsur_min,round(max(lsur)::numeric,2) as lsur_max, min(slsur) as slsur_min,max(slsur) as slsur_max, min(agwrc) as agwrc_min,max(agwrc) as agwrc_max from pwatparm2 where infilt > 0 and lsur > 1 and segstart not in (select segstart from perlndgeninfo where luname ilike '%water%')";
   $this->listobject->performQuery();
   $this->listobject->showList();



   $this->listobject->querystring = "select min(deepfr) as deepfr_min,max(deepfr) as deepfr_max, min(basetp) as basetp_min,max(basetp) as basetp_max, min(agwetp) as agwetp_min,max(agwetp) as agwetp_max from pwatparm3 where segstart not in (select segstart from perlndgeninfo where luname ilike '%water%')";
   $this->listobject->performQuery();
   $this->listobject->showList();



   $this->listobject->querystring = "select min(cepsc) as cepsc_min,max(cepsc) as cepsc_max, min(uzsn) as uzsn_min,max(uzsn) as uzsn_max, min(nsur) as nsur_min,max(nsur) as nsur_max, min(intfw) as intfw_min,max(intfw) as intfw_max, min(lzetp) as lzetp_min,max(lzetp) as lzetp_max, min(irc) as irc_min,max(irc) as irc_max from pwatparm4 where segstart not in (select segstart from perlndgeninfo where luname ilike '%water%')";
   $this->listobject->performQuery();
   $this->listobject->showList();

/*
   print("<br><b>PERLND Quality Params:</b><br>");
   $listobject->querystring = "select min(sqo) as sqo_min,max(sqo) as sqo_max,min(potfw) as potfw_min,max(potfw) as potfw_max,min(potfs) as potfs_min,max(potfs) as potfw_max, min(acqop) as acqop_min,max(acqop) as acqop_max, min(sqolim) as sqolim_min,max(sqolim) as sqolim_max, min(wsqop) as wsqop_min,max(wsqop) as wsqop_max, min(ioqc) as ioqc_min,max(ioqc) as ioqc_max, min(aoqc) as aoqc_min,max(aoqc) as aoqc_max from pqualinput where wsqop > $minwsqop";
   $listobject->performQuery();
   $listobject->showList();
   $listobject->querystring = "select * from pqualinput where wsqop > $minwsqop";
   $listobject->performQuery();
   #$listobject->showList();


   print("<br><b>IMPLND Quality Params:</b><br>");
   $listobject->querystring = "select min(acqop) as acqop_min,max(acqop) as acqop_max, min(sqolim) as sqolim_min,max(sqolim) as sqolim_max, min(wsqop) as wsqop_min,max(wsqop) as wsqop_max from iqualinput where wsqop > $minwsqop";
   $listobject->performQuery();
   #$listobject->showList();


   print("<br><b>Monthly Parameters:</b><br>");

   # get monthly parameters into a single table, then summarize
   $monar = array('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV', 'DEC');

   $listobject->querystring = "create temp table monparm (parmname varchar(64),parmval float8)";
   $listobject->performQuery();

   foreach($monar as $thismonth) {

      $listobject->querystring = "insert into monparm select 'LZETP',$thismonth from monlzetp where $thismonth > $minlzetp";
      $listobject->performQuery();

      $listobject->querystring = "insert into monparm select 'UZSN',$thismonth from monuzsn where $thismonth > $minuzsn";
      $listobject->performQuery();

      $listobject->querystring = "insert into monparm select 'CSECP',$thismonth from monintercep where $thismonth > $mincepsc";
      $listobject->performQuery();

      $listobject->querystring = "insert into monparm select 'NSUR',$thismonth from monmanning where $thismonth > $minnsur";
      $listobject->performQuery();

      $listobject->querystring = "insert into monparm select 'IMPLND_MON-ACCUM', $thismonth from imonaccum where $thismonth > 0";
      $listobject->performQuery();

      $listobject->querystring = "insert into monparm select 'IMPLND_MON-SQOLIM', $thismonth from imonsqolim where $thismonth > 0";
      $listobject->performQuery();

      $listobject->querystring = "insert into monparm select 'PERLND_MON-ACCUM', $thismonth from pmonaccum where $thismonth > 0";
      $listobject->performQuery();

      $listobject->querystring = "insert into monparm select 'PERLND_MON-SQOLIM', $thismonth from pmonsqolim where $thismonth > 0";
      $listobject->performQuery();

   }

   $listobject->querystring = "select parmname,min(parmval),max(parmval) from monparm group by parmname";
   $listobject->performQuery();
   $listobject->showList();

   print("<b>Param summary by landuse <br>");
   $listobject->querystring = "create temp table landuseparm2 as select substring(a.luname,strpos(a.luname,'_') + 1) as lutype, b.* from perlndgeninfo as a,pwatparm2 as b where a.segstart = b.segstart";
   $listobject->performQuery();
   #$listobject->querystring = "select * from landuseparm2 order by segstart";
   #$listobject->performQuery();
   #$listobject->showList();
   $listobject->querystring = "select lutype,min(lzsn) as lzsn_min,max(lzsn) as lzsn_max,min(infilt) as infilt_min,max(infilt) as infilt_max, min(lsur) as lsur_min,max(lsur) as lsur_max, min(slsur) as slsur_min,max(slsur) as slsur_max, min(agwrc) as agwrc_min,max(agwrc) as agwrc_max from landuseparm2 group by lutype order by lutype";
   $listobject->performQuery();
   $listobject->showList();

   $listobject->querystring = "create temp table landuseloads as select a.segstart,substring(a.luname,strpos(a.luname,'_') + 1) as lutype, b.JAN as janloads from perlndgeninfo as a,pmonaccum as b where a.segstart = b.segstart";
   $listobject->performQuery();
   print("Landuse query: $listobject->querystring <br>");
   #$listobject->querystring = "select * from landuseloads order by segstart";
   #$listobject->performQuery();
   #$listobject->showList();
   $listobject->querystring = "select lutype,sum(janloads) from landuseloads group by lutype";
   $listobject->performQuery();
   $listobject->showList();
   #$listobject->querystring = "select segstart,lutype,janloads from landuseloads where lutype like '%potential%'";
   #$listobject->performQuery();
   #$listobject->showList();

*/

   }


} /* end UCIObject */



# functions for HSPF

#######################################################################
###                        Global Variables                         ###
#######################################################################


#######################################################################
###                        Global Functions                         ###
#######################################################################

function otherfunction($outfile,$dbcols,$fielddelim) {

   # open the CDR file
   $inf = fopen($cdrfile,'r');

   # read the first line from the CDR file
   $inline = fgets($inf,5000);

   $varlist = split(",",$dbcols);
   $valuelist = split($fielddelim,$inline);

   $insertstring = '';
   $i = 0;
   $delim = '';
   foreach ($varlist as $varname) {
      $thisval = $valuelist[$i];
      $vartype = 'varchar(255)';
      if (is_numeric($thisval)) {$vartype = 'float8';}
      $insertstring .= "$delim$varname $vartype";
      #print("$thisval $vartype\n");
      $delim = ',';
      $i++;
   }

   return $insertstring;

}

function parseHSPFout($infilename,$thiscolumn) {
   # opens a standard HSPF output file
   # retrieves the specified column from the array
   # if -1 is given for data column, all columns are returned

   $inf = fopen($infilename,'r');
   $headarray = array();
   $datalines = array();
   #$thisdata = array();
   $i = 0;
   $delim = '';

   $dbcols = '';
   $maxlinewidth= 2000;
   # default value for the field delim
   $fielddelim = ' ';
   $dataline = 0;

   # stash lines in head array until you reach the data lines
   while (!($dataline)) {
      $inline = fgets($inf,$maxlinewidth);
      $thisline = array();
      $thisline = preg_split("/[\s,]+/",ltrim(chop($inline)));
      if ($thisline[1] == 'Date/time') { $dataline = 1; }
      $headarray[$i] = $inline;
      $i++;
   }
   $blankline1 = fgets($inf,$maxlinewidth);
   $blankline2 = fgets($inf,$maxlinewidth);

   #$formatstring = '%6[^\n]%4[^\n]%3[^\n]%3[^\n]%3[^\n]%3[^\n]%14[^\n]%14[^\n]%14[^\n]%14[^\n]%14[^\n]%14[^\n]%14[^\n]';

   #$formatstring = '%6s%4s%3s%3s%3s%3s%14s%14s%14s%14s%14s%14s%14s';
   $formatstring = '%6s%4d%3d%3d%3d%3d%14f%14f%14f%14f%14f%14f%14f';

   if ($inf) {
      while ( $inline = fgets($inf,$maxlinewidth) ) {

         #$thisline = preg_split("/[\s,]+/",ltrim(chop($inline)));
         $thisline = sscanf($inline,$formatstring);
         if ($thisline[4] == 24) { $thisline[4] = '00'; }
         $thisdate = "$thisline[1]-$thisline[2]-$thisline[3] $thisline[4]:$thisline[5]:0";
         if ($firstdate == '') { $firstdate = $thisdate;}
         if ($thiscolumn == -1) {
            $thisdata = array_slice($thisline,6);
         } else {
           $thisdata = $thisline[(6 + $thiscolumn - 1)];
         }
         array_push($datalines,array($thisdate,$thisdata));

      }
   } else {
      print("Error: $infilename can not be opened<br>");
   }

   $lastdate = $thisdate;

   fclose($inf);

   $outarr = array($headarray,$datalines);
   return $outarr;

} /* end parseHSPFout */



function parseHSPFMultiout($infilename,$thiscolumn, $totaldatacols) {
   # opens a standard HSPF output file
   # retrieves the specified column from the array
   # if -1 is given for data column, all columns are returned

   $inf = fopen($infilename,'r');
   $headarray = array();
   $datalines = array();
   #$thisdata = array();
   $i = 0;
   $delim = '';

   $dbcols = '';
   $maxlinewidth= 2000;
   # default value for the field delim
   $fielddelim = ' ';
   $dataline = 0;

   # stash lines in head array until you reach the data lines
   while (!($dataline)) {
      $inline = fgets($inf,$maxlinewidth);
      $thisline = array();
      $thisline = preg_split("/[\s,]+/",ltrim(chop($inline)));
      if ($thisline[1] == 'Date/time') { $dataline = 1; }
      $headarray[$i] = $inline;
      $i++;
   }
   $blankline1 = fgets($inf,$maxlinewidth);
   $blankline2 = fgets($inf,$maxlinewidth);

   #$formatstring = '%6[^\n]%4[^\n]%3[^\n]%3[^\n]%3[^\n]%3[^\n]%14[^\n]%14[^\n]%14[^\n]%14[^\n]%14[^\n]%14[^\n]%14[^\n]';

   #$formatstring = '%6s%4s%3s%3s%3s%3s%14s%14s%14s%14s%14s%14s%14s';
   $formatstring = '%6s%4d%3d%3d%3d%3d';
   for ($i = 1; $i <= $totaldatacols; $i++) {
      $formatstring .= '%14f';
   }

   if ($inf) {
      while ( $inline = fgets($inf,$maxlinewidth) ) {

         #$thisline = preg_split("/[\s,]+/",ltrim(chop($inline)));
         $thisline = sscanf($inline,$formatstring);
         if ($thisline[4] == 24) {
            $thisline[4] = '23';
            $thisline[5] = '59';
         }
         $thisdate = "$thisline[1]-$thisline[2]-$thisline[3] $thisline[4]:$thisline[5]:0";
         if ($firstdate == '') { $firstdate = $thisdate;}
         if ($thiscolumn == -1) {
            $thisdata = array_slice($thisline,6);
         } else {
           $thisdata = $thisline[(6 + $thiscolumn - 1)];
         }
         array_push($datalines,array_merge($thisdate,$thisdata));

      }
   } else {
      print("Error: $infilename can not be opened<br>");
   }

   $lastdate = $thisdate;

   fclose($inf);

   $outarr = array($headarray,$datalines);
   return $outarr;

} /* end parseHSPFMultiout */


function makeUCITable($dbobj,$ucisection,$datalines,$ucitables,$debug, $cleartable=0) {

   $tableinfo = $ucitables["$ucisection"];
   if ($debug) {
      print("Adminsetup array info for ($ucisection)<br>");
      print_r($tableinfo);
   }

   $tabledef = $tableinfo['tabledef'];
   $tablename = $tableinfo['tablename'];
   $paramnulls = array();
   if (in_array('paramnulls', array_keys($tableinfo))) {
      $paramnulls = $tableinfo['paramnulls'];
   }

   $dbobj->querystring = $tabledef;
   if ($debug) {
      print("Create Statement for ($ucisection): $tabledef<br>\n");
   }
   if (!$dbobj->tableExists($tablename)) {
      $dbobj->performquery();
   }
   # delete any existing records if instructed
   if ($cleartable) {
      $dbobj->querystring = "delete from $tablename";
      $dbobj->performquery();
   }

   foreach($datalines as $thisline) {
      $k = 0;
      foreach($paramnulls as $thisnull) {
         if ($thisline[$k] == $thisnull['nullstr']) {
            $thisline[$k] = $thisnull['defval'];
         }
         $k++;
      }
      array_walk($thisline,'ltrim');
      array_walk($thisline,'rtrim');
      array_walk($thisline,'listToSQL');
      $linevals = implode(",",$thisline);
      $dbobj->querystring = "insert into $tablename values ($linevals)";
      if ($debug) {
         print("$dbobj->querystring<br>\n");
      }
      $dbobj->performquery();
   }

   # test to see if it is a monthly-table, if so, summarize monthly values for an annual mean
   if ($tableinfo['ismonthly']) {
      if ($debug) {
         print("Debug: Grabbing monthly average \n");
      }
      $meancol = $tableinfo['annual_name'];
      if ($dbobj->columnExists($tablename, $meancol)) {
         $dbobj->querystring = "update $tablename set $meancol = (jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec ) / 12.0";
         if ($debug) {
            print("$dbobj->querystring \n");
         }
         $dbobj->performQuery();
      }
   }
}

function parseFixedWidthUCI($infilename,$ucisection,$ucisubsection,$formatstring,$debug) {
   # opens a standard HSPF .uci file, returns the desired section data
   # as parsed lines in an array
   # stows the UCI in a db if $dbobj is > -1
   # $formatstring = contains sprintf type formatting for parseing the lines,
   #   refer to global variables block at the top of this file for common UCI section
   #   format string

   $inf = @fopen($infilename,'r');
   if (!$inf) {
      return array();
   } else {
   $sectiondata = array();
   $i = 0;
   $k = 0;
   $delim = '';
   $dbcols = '';
   $maxlinewidth= 2000;
   $endofsection = 0;
   $startofsection = 0;
   $mainsection = 0;

   if ($debug) {print ("Format String: $formatstring <br>"); }

   # if $ucisubsection is blank, then just start parsing
   if ($ucisubsection == '') {
      $ucisubsection = $ucisection;
      $startofsection = 1;
   }


   # skip till main section is reached (which contains the sub-section)
   while ( ( !($mainsection) and ($inline = fgets($inf,$maxlinewidth)) ) ) {

      #do nothing till main section is reached
      if ( !(strstr($inline,'***')) ) {
         # note, since major section openings are left justified
         # we do not perform an "ltrim" on the inline at this time
         $thisline = preg_split("/[\s,]+/",chop($inline));
         # test for section open label
       if ($thisline[0] == $ucisection) {
          $mainsection = 1;
          if ($debug) { print("Found $ucisection\n<br>"); }
         }
      }
   }


   # skip down until you reach the desired sub-section
   while ( ( !($endofsection) and ($inline = fgets($inf,$maxlinewidth)) ) ) {

      $k++;

      $thisline = array();
      # test for comment line, if so, leave it out
      if ( !(strstr($inline,'***')) and !(strlen(ltrim($inline)) == 0) ) {
         $thisline = sscanf($inline,"$formatstring");

         # test for end of section
         if ( strstr($inline,"END $ucisubsection") ) {
          $endofsection = 1;
         }
         if (!$startofsection) {
            # not at desired section ,nothing to be done
            ;
         } elseif (!$endofsection) {
            # first eliminate extraneous whitespace
            array_walk($thisline,'ltrim');
            array_walk($thisline,'rtrim');
            # at desired section, store parsed line in array
            if (strlen($thisline[0]) > 0 ) {
               $sectiondata[$i] = $thisline;
            }
            # output for debugging purposes
            if ($debug) {
               print("Line $i: ");
               print(" RAW: $inline <br>");
               print_r(array_values ($thisline));
               print("<br>");
            }
            $i++;
         }

         # test for section open label
         if ( (strstr($inline,$ucisubsection) and !$endofsection ) ) {
            $startofsection = 1;
            if ($debug) { print("Found $ucisubsection\n<br>"); }
         }
      }
   }

   if ($debug) {
      print("$k lines scanned.\n");
   }

   fclose($inf);
   return $sectiondata;
   }

} /* end parseFixedWidthUCI */

function parseMultiFixedWidthUCI($infilename,$ucisection,$ucisubsection,$formatstring,$debug) {
   # opens a standard HSPF .uci file, returns the desired section data
   # as parsed lines in an array
   # stows the UCI in a db if $dbobj is > -1
   # $formatstring = contains sprintf type formatting for parseing the lines,
   #   refer to global variables block at the top of this file for common UCI section
   #   format string
   # differs from parseFixedWidthUCI in that it will keep looking for multiple instances of the
   # same table

   $inf = @fopen($infilename,'r');
   if (!$inf) {
      return array();
   } else {
   $sectiondata = array();
   $i = 0;
   $k = 0;
   $delim = '';
   $dbcols = '';
   $maxlinewidth= 2000;
   $endofsection = 0;
   $startofsection = 0;
   $mainsection = 0;

   if ($debug) {print ("Format String: $formatstring <br>"); }

   # if $ucisubsection is blank, then just start parsing
   if ($ucisubsection == '') {
      $ucisubsection = $ucisection;
      $startofsection = 1;
   }


   $eof = 0;
   $eom = 0;
   $numss = 0;
   # loop until end of file or end of main section
   while ( !($eof or $eom) ) {
      # skip till main section is reached (which contains the sub-section)
      while ( ( !($mainsection or $eom) and ($inline = fgets($inf,$maxlinewidth)) ) ) {
         if ( strstr($inline,"END $ucisection") ) {
            $eom = 1;
            print("Found END $ucisection\n<br>");
         }
         #do nothing till main section is reached
         if ( !(strstr($inline,'***')) ) {
            # note, since major section openings are left justified
            # we do not perform an "ltrim" on the inline at this time
            $thisline = preg_split("/[\s,]+/",chop($inline));
            # test for section open label
          if ($thisline[0] == $ucisection) {
             $mainsection = 1;
             if ($debug) { print("Found $ucisection\n<br>"); }
            }
         }
      }
      $endofsection = 0;
      $startofsection = 0;
      $i = 0;


      # skip down until you reach the desired sub-section
      while ( ( !($endofsection) and ($inline = fgets($inf,$maxlinewidth)) ) ) {

         $k++;

         $thisline = array();
         # test for comment line, if so, leave it out
         if ( !(strstr($inline,'***')) and !(strlen(ltrim($inline)) == 0) ) {
            $thisline = sscanf($inline,"$formatstring");

            # test for end of section
            if ( strstr($inline,"END $ucisubsection") ) {
             $endofsection = 1;
             if ($debug) { print("Found END $ucisubsection\n<br>"); }
            }
            if (!$startofsection) {
               # not at desired section ,nothing to be done
               ;
            } elseif (!$endofsection) {
               # first eliminate extraneous whitespace
               array_walk($thisline,'ltrim');
               array_walk($thisline,'rtrim');
               # at desired section, store parsed line in array
               if (strlen($thisline[0]) > 0 ) {
                  $sectiondata[$numss][$i] = $thisline;
               }
               # output for debugging purposes
               if ($debug) {
                  print("Line $i: ");
                  print(" RAW: $inline <br>");
                  print_r(array_values ($thisline));
                  print("<br>");
               }
               $i++;
            }

            # test for section open label
            if ( (strstr($inline,$ucisubsection) and !$endofsection ) ) {
               $startofsection = 1;
               if ( !( ltrim(rtrim($inline)) == $ucisubsection ) ) {
                  # there is something after the ucisubsection tag, like a number perhaps,
                  # get it for our array label
                  $tokens = preg_split("/[\s,]+/",chop($inline));
                  $numss = $tokens[ count($tokens)-1 ];
               } else {
                  $numss++;
               }
               if ($debug) { print("Found $ucisubsection\n<br>"); }
            }
         }
      }

      $eof = !($inline);
   } /* end of file reached */

   if ($debug) {

      if ($eof) {
         print("Reached End of File.\n");
      }
      print("$k lines scanned.\n");
   }

   fclose($inf);
   return $sectiondata;
   }

} /* end parseMultiFixedWidthUCI */


function parseUCIpart($infilename,$ucisection,$ucisubsection) {
   # opens a standard HSPF .uci file, returns the desired section data
   # as parsed lines in an array

   $inf = fopen($infilename,'r');
   $sectiondata = array();
   $i = 0;
   $k = 0;
   $delim = '';
   $dbcols = '';
   $maxlinewidth= 2000;
   # default value for the field delim is a space ' '
   $fielddelim = ' ';
   $endofsection = 0;
   $startofsection = 0;
   $mainsection = 0;

   # skip till main section is reached (which contains the sub-section)
   while ( ( !($mainsection) and ($inline = fgets($inf,$maxlinewidth)) ) ) {

      #do nothing till main section is reached
      if ( !(strstr($inline,'***')) ) {
         # note, since major section openings are left justified
         # we do not perform an "ltrim" on the inline at this time
         $thisline = preg_split("/[\s,]+/",chop($inline));
         # test for section open label
       if ($thisline[0] == $ucisection) {
          $mainsection = 1;
          #print("Found $ucisection\n");
         }
      }
   }


   # skip down until you reach the desired sub-section
   while ( ( !($endofsection) and ($inline = fgets($inf,$maxlinewidth)) ) ) {

      $k++;

      $thisline = array();
      # test for comment line, if so, leave it out
      if ( !(strstr($inline,'***')) ) {
         $thisline = preg_split("/[\s,]+/",ltrim(chop($inline)));

         # test for end of section
         if ( ($thisline[0] == "END") and (($thisline[1] == $ucisubsection) or ($thisline[1] == $ucisection)) ) {
          $endofsection = 1;
         }
         if (!$startofsection) {
            # not at desired section ,nothing to be done
            ;
         } elseif (!$endofsection) {
            # at desired section, store parsed line in array
            $sectiondata[$i] = $thisline;
            $i++;
         }

         # test for section open label
         if ( ($thisline[0] == $ucisubsection) or ($ucisubsection == '') ) {
            $startofsection = 1;
            #print("Found $ucisubsection\n");
         }
      }
   }

   print("$k lines scanned.\n");
   fclose($inf);
   return $sectiondata;

} /* end parseUCIpart */

   /* begin createExtSourceBlock */
function createExtSourceBlock($dbobj,$geninfo,$landtype,$wdm,$wstbl,$wsidcol,$wsgeomcol,$ptbl,$pidcol,$pgeomcol) {

   # this function can handle perlands or implands

   #initialize output variable
   $extsourcelist = array();

   ###################################################################
   # create temp table to stash subwshed/landid mappings
   ###################################################################
   $cquery = "create temp table templands (swsid integer, landid integer)";
   $dbobj->querystring = $cquery;
   #print("$cquery\n<br>");
   $dbobj->performQuery();

   ###################################################################
   # insert all subwsid and land id pairs for this uci into map table
   ###################################################################
   foreach ($geninfo as $segline) {

      $segtxt = $segline[1];
      $segarr = split('_',$segtxt);
      $swsid = $segarr[0];
      $perlandid = $segline[0];
      $c = count($segline);
      $istring = "insert into templands(swsid,landid) values ($swsid, $perlandid)";
      $dbobj->querystring = $istring;
      $dbobj->performQuery();
      #print("$istring\n<br>");

   }

   ###################################################################
   # create summary table with min and max land id for each subshed
   ###################################################################
   $mapquery = "create temp table hiloperland as select swsid,min(landid) as loperland,max(landid) as hiperland from templands group by swsid";
   #print("$mapquery<br>");
   $dbobj->querystring = $mapquery;
   $dbobj->performQuery();

   $qstring = "select a.$wsidcol, b.$pidcol,c.loperland,c.hiperland, round( (area2d(intersection(geometryn(a.$wsgeomcol,0),geometryn(b.$pgeomcol,0)))/area2d(a.$wsgeomcol))::numeric ,4) as weight from $wstbl as a,$ptbl as b, hiloperland as c where (a.$wsgeomcol && b. the_geom) and area2d(intersection(geometryn(a.$wsgeomcol,0), geometryn(b.$pgeomcol,0))) > 0 and a.$wsidcol = c.swsid order by loperland";
   #print("$qstring<br>");
   $dbobj->querystring = $qstring;
   $dbobj->maxrecords = 1000;
   $dbobj->performQuery();


   $n = count($dbobj->queryrecords);
   #print("$n records returned<br>");

   # write header
   $hstring = '<Name>   x <Name> x tem strg<-factor->strg <Name>   x   x        <Name> x x ***';
   #fwrite($fp,"$hstring\r");
   array_push($extsourcelist,$hstring);


   for ($i = 1; $i <= $n; $i++) {
      $staid = $dbobj->getRecordValue($i,"$pidcol");
      $wsid = $dbobj->getRecordValue($i,"$wsidcol");
      $weight = $dbobj->getRecordValue($i,'weight');
      $lop = $dbobj->getRecordValue($i,'loperland');
      $hip = $dbobj->getRecordValue($i,'hiperland');

      #print("$wdm, $staid, $wsid, $weight<br>");

      # HSPF .uci format is as follows
      # *** sw 1-8,10 : Appomattox(101)
      #           WDM3   243 PREC     ENGL        0.5010     PERLND 110 162 EXTNL  PREC
      $fstring = '%-6s%4s PREC     ENGLZERO%4.4f SAME %6s %-3s %-3s EXTNL  PREC';
      $outstring = sprintf($fstring, $wdm,$staid,$weight,$landtype,$lop,$hip);
      #fwrite($fp,"$outstring\r");
      array_push($extsourcelist,$outstring);
   }
   #print("<a href=./logs/ext_sources.uci>Output File</a>");

   # clean up data tables in case we are called again within this session
   $dbobj->querystring = "drop table hiloperland";
   $dbobj->performQuery();
   $dbobj->querystring = "drop table templands";
   $dbobj->performQuery();

   return $extsourcelist;

} /* end createExtSourceBlock */

function createWDMUtilExpFormat($dataarray,$datefield,$datafield,$location,$datatype,$scenario,$description) {
   /* creates an .exp type format, which should be easily importable into WDMUtil */

   $outarray = array();
   array_push($outarray,"$scenario");
   array_push($outarray,"$location");
   array_push($outarray,"$datatype");
   array_push($outarray,"$description");
   array_push($outarray,'Date            DSN  1');

   foreach ($dataarray as $dataline) {
      $datecol = $dataline[$datefield];
      /*
         $thisdate = getdate(strtotime($datecol));
         $mo = $thisdate['mon'];
         $day = $thisdate['mday'];
         $year = $thisdate['year'];
      */
      list($year,$mo,$daytime) = split('-',$datecol);
      list($day,$time) = split(' ',$daytime);
      $hr = $time + 1; /* converts from ODBC convention to HSPF convention */
      $thisvalue = $dataline[$datafield];
      if ($time <> '') {
         # hourly, include hour field
         $pformat = '%4s/%2s/%2s %2s %3.4f';
         $outline = sprintf($pformat,$year,$mo,$day,$hr,$thisvalue);
      } else {
         # daily, do not include hour field
       $pformat = '%4s/%2s/%2s    %3.4f';
         $outline = sprintf($pformat,$year,$mo,$day,$thisvalue);
     }

     array_push($outarray,$outline);
   }

   return $outarray;

} /* end createWDMUtilExpFormat() */

function createPlotGenInserts($listobject,$infile,$ucitables,$localplots,$srcentity,$srcgrp,$srcparam,$factor,$debug) {

# a fairly generic function which will create plotgen entries for a list of entities
# (RCHRES,COPY,PERLND etc)
# returns:
   # FILES $filesblock
   # OPN SEQUENCE $opnblock
   # PLOTINFO $pltgenblock
   # SCEMATIC $schematicblock
   # MASSLINK $masslinkblock

   if ($debug) { print("$filebase <br>"); }

   # parse UCI for FILES block
   # $ucitables is declared in HSPF function file
   $geninfo = parseFixedWidthUCI($infile,'FILES','',$ucitables["FILES"]["uciformat"],$debug);
   makeUCITable($listobject,'FILES',$geninfo,$ucitables,$debug);


   ##########################################################################
   # create a  table of open file handles between 30 and 99 for pltgen files
   ##########################################################################

   $pltgenfiles = '';
   $listobject->querystring = "create temp table openhandles (fileno integer)";
   $listobject->performquery();

   if ($debug) {
      print("$listobject->querystring <br>");
   }

   for ($i = 30;$i <= 99;$i++) {
      $listobject->querystring = "insert into openhandles values($i)";
      $listobject->performquery();
      if ($debug) {
           print("$listobject->querystring <br>");
      }
   }

   if ($debug) {
         $listobject->querystring = "select * from openhandles";
         $listobject->performquery();
         $listobject->showlist();
   }

   $listobject->querystring = "delete from openhandles where fileno in (select fileno from files where fileno <= 99 and fileno >= 30)";
   $listobject->performquery();

   # debug info
   $listobject->querystring = "select * from openhandles order by fileno desc";
   $listobject->performquery();

   if ($debug) {
      $listobject->showlist();
   }

   $openar = $listobject->queryrecords;
   $openhandles = array();

   foreach ($openar as $thisopen) {
      array_push($openhandles,$thisopen["fileno"]);
   }

   if ($debug) { $listobject->showlist();}

   ##########################################################################
   ###              end open file handle list                             ###
   ##########################################################################


   ##########################################################################
   # find the next available plotgen number
   ##########################################################################
      # parse UCI for PLOTINFO block
      $geninfo = parseFixedWidthUCI($infile,'PLTGEN','PLOTINFO',$ucitables["PLOTINFO"]["uciformat"],$debug);
      makeUCITable($listobject,'PLOTINFO',$geninfo,$ucitables,$debug);

      $listobject->querystring = "select max(segstart) from plotinfo";
      $listobject->performquery();
      if ($debug) { $listobject->showlist();}
      $nextplotid = $listobject->getRecordValue(1, 'max');
      $nextplotid = intval($nextplotid);
      $nextplotid++;

      if ($debug) { print("Next available plot ID: $nextplotid <br>");}

   ##########################################################################
   ###              end fund next plotgen id                              ###
   ##########################################################################

   $opnblock = array();
   array_push($opnblock,"*** begin OPN SEQUENCE inserts ***");
   $pltgenblock = array();
   array_push($pltgenblock,"*** begin PLOTINFO inserts ***");
   $filesblock = array();
   array_push($filesblock,"*** begin FILES inserts ***");
   $schematicblock = array();
   array_push($schematicblock,"*** begin SCHEMATIC inserts ***");

   ####### Create OPN SEQUENCES for new PLOTGEN
   print("$localplots <br>");
   $newplots = split(',',$localplots);
   $i = 0;
   foreach ($newplots as $plotreach) {
      $thisreach = rtrim(ltrim($plotreach));
      $thishandle = array_pop($openhandles);
      $opnline = sprintf($ucitables["OPN SEQUENCE"]["uciprint"],'PLTGEN',$nextplotid + $i);
      if ($debug) { print("$opnline <br>"); }
      array_push($opnblock,$opnline);
      $fileline = sprintf($ucitables["FILES"]["uciprint"],'',$thishandle,"$srcentity.localflow.$thisreach.out");
      array_push($filesblock,$fileline);
      $plotinfoline = sprintf($ucitables["PLOTINFO"]["uciprint"],$nextplotid + $i,'',$thishandle,0,1,0,9,24,1);
      array_push($pltgenblock,$plotinfoline);
      $schematicline = sprintf($ucitables["SCHEMATIC"]["uciprint"],$srcentity, $thisreach, '', '','PLTGEN',($nextplotid + $i),$mlno);
      array_push($schematicblock,$schematicline);
      $i++;

   }

   array_push($opnblock,"*** end OPN SEQUENCE inserts ***");
   array_push($pltgenblock,"*** end PLOTINFO inserts ***");
   array_push($filesblock,"*** end FILES inserts ***");


   # format MASS-LINK block for aggregating
   $masslinkblock = array();
   array_push($masslinkblock,"*** begin MASS-LINK inserts ***");
   array_push($masslinkblock,"  MASS-LINK       $mlno");
   array_push($masslinkblock,"*** this mass-link will aggregate SURO,AGWO, and IFWO for the block directed to it");
   array_push($masslinkblock,"*** output is converted into cfs from in/intvl");
   array_push($masslinkblock,"<-Volume-> <-Grp> <-Member-><--Mult-->     <-Target vols> <-Grp> <-Member->  ***");
   array_push($masslinkblock,"<Name>            <Name> x x<-factor->     <Name>                <Name> x x  ***");
   array_push($masslinkblock,sprintf("%6s     %-6s %-6s 1  %5.4f     PLTGEN         INPUT  MEAN   1",$srcentity,$srcgrp,$srcparam,$factor) );
   array_push($masslinkblock,"  END MASS-LINK   $mlno");
   array_push($masslinkblock,"*** end MASS-LINK inserts ***");

   $returnarray = array("FILES"=>$filesblock,
                        "OPN SEQUENCE"=>$opnblock,
                        "PLOTINFO"=>$pltgenblock,
                        "SCHEMATIC"=>$schematicblock,
                        "MASS-LINK"=>$masslinkblock
                        );


   # clean up
   $listobject->querystring = "drop table openhandles";
   $listobject->performquery();
   $listobject->querystring = "drop table plotinfo";
   $listobject->performquery();
   $listobject->querystring = "drop table files";
   $listobject->performquery();


   return $returnarray;

} /* end createPlotGenInserts */

function makeBlankDSN($dsn,$tstype,$tc) {


  switch ($tc) {

  case 3:
  # hourly
  $tlabel = 'hourly';
  break;

  case 4:
  # daily
  $tlabel = ' daily';
  break;

  default:
  # hourly
  $tlabel = 'hourly';
  break;

  }

  $dsnar = array();
  array_push($dsnar,"DATE    ");
  array_push($dsnar,"WDMSFL  ");
  array_push($dsnar,"SYSTEM  ");
  array_push($dsnar,"COMMENT ");
  array_push($dsnar,"END COMMENT ");
  array_push($dsnar,sprintf("DSN         %4s    TYPE  TIME   NDN  10   NUP  10   NSA  30   NSP 100   NDP 300",$dsn));
  array_push($dsnar,"  LABEL   ");
  array_push($dsnar,"    SEADBG 858796082                                                            ");
  array_push($dsnar,"    SEADND 858796082                                                       ");
  array_push($dsnar,"    TSTYPE  $tstype                                                             ");
  array_push($dsnar,"    TGROUP         6                                                       ");
  array_push($dsnar,"    COMPFG         1                                                       ");
  array_push($dsnar,"    TSFORM         1                                                       ");
  array_push($dsnar,"    TCODE          $tc                                                       ");
  array_push($dsnar,"    TSSTEP         1                                                       ");
  array_push($dsnar,"    VBTIME         1                                                       ");
  array_push($dsnar,"    TSBYR       1970                                                       ");
  array_push($dsnar,"    IDSCEN  $tstype                                                             ");
  array_push($dsnar,"    IDCONS  $tstype                                                             ");
  array_push($dsnar,"    IDLOCN  $tstype$dsn                                                            ");
  array_push($dsnar,"    STANAM  $tlabel Data Template                                           ");
  array_push($dsnar,"    DCODE          0                                                       ");
  array_push($dsnar,"  END LABEL   ");
  array_push($dsnar,"  DATA       STARTS: 1976  1  1  0  0  0  ENDS: 1976  1  1  1  0  0");
  array_push($dsnar,"    1975 12 31 24  0  0     $tc  1  0     1  0        0.00000    ");
  array_push($dsnar,"  END DATA");
  array_push($dsnar,"END DSN ");

  return $dsnar;

}

function createWDIMEXDSN($dsn,$tstype,$tc,$dsndata,$datecol,$datacol,$mindate,$maxdate) {


  list($maxyear,$mo,$daytime) = split('-',$maxdate);
  list($minyear,$mo,$daytime) = split('-',$mindate);

  switch ($tc) {

  case 3:
  # hourly
  $tlabel = 'hourly';
  $tdur = 1;
  $tco = 3;
  break;

  case 4:
  # daily
  $tlabel = ' daily';
  $tdur = 1;
  $tco = 4;
  break;

  case 6:
  # daily
  $tlabel = ' daily';
  $tdur = 1;
  $tco = 5;
  break;

  default:
  # hourly
  $tlabel = 'hourly';
  $tdur = 1;
  $tco = 3;
  break;

  }

  $dsnar = array();
  array_push($dsnar,"DATE    ");
  array_push($dsnar,"WDMSFL  ");
  array_push($dsnar,"SYSTEM  ");
  array_push($dsnar,"COMMENT ");
  array_push($dsnar,"END COMMENT ");
  array_push($dsnar,sprintf("DSN         %4s    TYPE  TIME   NDN  10   NUP  10   NSA  30   NSP 100   NDP 300",$dsn));
  array_push($dsnar,"  LABEL   ");
  array_push($dsnar,"    SEADBG 858796082                                                            ");
  array_push($dsnar,"    SEADND 858796082                                                       ");
  array_push($dsnar,"    TSTYPE  $tstype");
  array_push($dsnar,"    TGROUP         6                                                       ");
  array_push($dsnar,"    COMPFG         1                                                       ");
  array_push($dsnar,"    TSFORM         1                                                       ");
  array_push($dsnar,"    TCODE          $tco                                                       ");
  array_push($dsnar,"    TSSTEP         1                                                       ");
  array_push($dsnar,"    VBTIME         1                                                       ");
  array_push($dsnar,"    TSBYR       1970                                                       ");
  array_push($dsnar,"    IDSCEN  $tstype                                                             ");
  array_push($dsnar,"    IDCONS  $tstype                                                             ");
  array_push($dsnar,"    IDLOCN  $tstype$dsn                                                            ");
  array_push($dsnar,"    STANAM  $tlabel Data Template                                           ");
  array_push($dsnar,"    DCODE          0                                                       ");
  array_push($dsnar,"  END LABEL   ");
  array_push($dsnar,"  DATA       STARTS: $minyear  1  1  0  0  0  ENDS: $maxyear  1  1  1  0  0");
  $dyear = $minyear - 1;
  array_push($dsnar,"    $dyear 12 31 24  0  0     3  1  0     1  0        0.00000");

  foreach($dsndata as $thisline) {
     $thisdate = $thisline[$datecol];
     $thisdata = $thisline[$datacol];
     list($year,$mo,$daytime) = split('-',$thisdate);
     list($day,$time) = split(' ',$daytime);
     list($hr,$min,$sec) = split(':',$time);
     if ($time == '') {
        $hr = 1;
        $min = 0;
        $sec = 0;
     }
     $outline = sprintf("    %4s %2s %2s %2s %2s %2s     %1s  1 31    %2d  1  %8.4f",$year,$mo,$day,$hr,$min,$sec,$tco,$tdur,$thisdata);
     array_push($dsnar,$outline);
     #print("$year,$mo,$day,$hr,$min,$sec,$tc,$thisdata");
  }
  array_push($dsnar,"  END DATA");
  array_push($dsnar,"END DSN ");

  return $dsnar;

}

function createMutsin($dsndata,$datecol,$datacol1,$datacol2) {


  $dsnar = array();
  for ($i = 1;$i <= 24; $i++) {
     # add header lines
     array_push($dsnar,"");
  }

  array_push($dsnar,"Year      Mo DayHr MinFlow (cfs)    FC (cfu/hr)");

  foreach($dsndata as $thisline) {
     $thisdate = $thisline[$datecol];
     $thisdata1 = $thisline[$datacol1];
     if (isset($datacol2)) {
        $thisdata2 = $thisline[$datacol2];
     } else {
        $thisdata2 = 0;
     }
     list($year,$mo,$daytime) = split('-',$thisdate);
     list($day,$time) = split(' ',$daytime);
     list($hr,$min,$sec) = split(':',$time);
     if ($time == '') {
        $hr = 1;
        $min = 0;
        $sec = 0;
     }
     $outline = sprintf("      %4s %2s %2s %2s %2s %7.5f %10.2f",$year,$mo,$day,$hr,$min,$thisdata1,$thisdata2);
     array_push($dsnar,$outline);
     #print("$year,$mo,$day,$hr,$min,$sec,$tc,$thisdata");
  }

  return $dsnar;

}

function createMultiMutsin($dsndata, $datecol, $datacols, $datalabel, $headertext, $numheader, $datatype) {


  $dsnar = array();
  for ($i = 1;$i <= ($numheader - 1); $i++) {
     # add header lines
     array_push($dsnar,"$headertext");
  }

  if (strlen($datatype) > 4) {
     $datatype = substr($datatype, 0,4);
  }

  $countpips = intval(count($dsndata) / 255);
  if ($countpips < 1) { $countpips = 1 ; }
  $counter = 0;

  print("Format: $mutformat<br>$datalabel<br>$headerlines<br>");

  array_push($dsnar,$datalabel);

  foreach($dsndata as $thisline) {

     $counter++;
     if ( intval($counter/$countpips) == ($counter/$countpips) ) {
        print('.');
     }
     $thisdate = $thisline[$datecol];

     list($year,$mo,$daytime) = split('-',$thisdate);
     list($day,$time) = split(' ',$daytime);
     list($hr,$min,$sec) = split(':',$time);
     if ($time == '') {
        $hr = 1;
        $min = 0;
        $sec = 0;
     }

     $outline = sprintf('%-4s %5d%3d%3d%3d%3d', $datatype,$year,$mo,$day,$hr,$min);

     for ($i = 1;$i <= count($datacols);$i++ ) {

        $colname = $datacols[($i - 1)];
        $thisdata = $thisline[$colname];
        $dform = sprintf("  %.7e", $thisdata);
        $dout = str_pad($dform,14, ' ' , STR_PAD_LEFT);
        $outline .= "$dout";
        #print("$thisdata<br>");
     }


     array_push($dsnar,$outline);
     #print("$outline");

     #print_r($outarray);
  }
  print("<br>");
  return $dsnar;

}


function mut2WDM($listobject, $uciobject, $basepath, $startdate, $enddate, $mutdir, $wdmfile) {


if (strlen($wdmfile) > 0) {

   # set up db connection
   $listobject = new pgsql_QueryObject;
   $listobject->dbconn = $dbconn;

   $indir = './uci';
   $infile = "$indir/$ucifile";
   $extsourcefile = "./logs/extsource.$ucifile";
   $outfile = "./logs/wq.$ucifile";

   if ($mutdir <> '') {
      $thismutdir = $mutdir;
   } else {
      $thismutdir = "cmut";
   }

   $thisstart = strftime("%Y/%m/%d",strtotime($startdate));
   $thisend = strftime("%Y/%m/%d",strtotime($enddate));

    $mutfiles = getFileArray("$basepath/$thismutdir",'.mut');
    $wdmfiles = getFileArray($basepath,'.wdm');

    rsort($mutfiles, SORT_NUMERIC);
    print_r($mutfiles);
    #sort($mutfiles);
    #print_r($mutfiles);

   $uciobject = new HSPF_UCIobject;
   $uciobject->ucidir = $indir;
   $uciobject->uciname = $ucifile;
   $uciobject->listobject = $listobject;
   $uciobject->debug = 0;
   #$uciobject->init();

   $impfile = array();
   $ucistorun = array();

   $ucino = 0;
   while (count($mutfiles) > 0) {
       $ucino++;
      $uciarr = array();
      $exttargets = array();
      $mutsinfo = array();
      $openseq = array();
      array_push($uciarr,"RUN");
      array_push($uciarr," ");
      array_push($uciarr,"GLOBAL");
      array_push($uciarr,"  UCI Created by WinHSPF for mdone07");
      array_push($uciarr,"  START       $thisstart 01:00  END    $thisend 18:00");
      array_push($uciarr,"  RUN INTERP OUTPT LEVELS    1    0");
      array_push($uciarr,"  RESUME     0 RUN     1                          UNITS      1");
      array_push($uciarr,"END GLOBAL");
      array_push($uciarr," ");
      array_push($uciarr,"FILES");
      array_push($uciarr,"<FILE>  <UN#>***<----FILE NAME ------------------------------------------------->");
      array_push($uciarr,"MESSU      24   $basepath/$ucibase$ucino.ech");

      array_push($uciarr,sprintf($uciobject->ucitables['FILES']['uciprint'],'WDM1',25,"$basepath/$wdmfile"));

      for ($i = 30;( ($i < 99) and (count($mutfiles) > 0) ) ;$i++) {
         $thismut = array_pop($mutfiles);
         array_push($uciarr,sprintf($uciobject->ucitables['FILES']['uciprint'],'',$i,"$basepath/$thismutdir/$thismut"));
         list($filebase,$ext) = split("\.",$thismut);
         #$nolets = intval(eregi_replace("[a-z]", "", $filebase));
         $nolets = eregi_replace("[a-z]", "", $filebase);
         $thisdsn = intval(str_replace("_", "", $nolets));
         #print("Orig: $filebase, numeric: $nolets<br>");
         $thisdsn += $dsnoff;
         $impfile =  array_merge($impfile,makeBlankDSN($thisdsn,"FC",3));
         array_push($impfile," ");
         array_push($exttargets,sprintf("MUTSIN %3d OUTPUT MEAN     1   %1.5f     WDM1  %4d FC     1 ENGL AGGR REPL",$i,$cfact,$thisdsn) );
         array_push($mutsinfo,sprintf($uciobject->ucitables['MUTSINFO']['uciprint'],$i,'',$i,0,1,25,3));
         array_push($openseq,sprintf($uciobject->ucitables['OPN SEQUENCE']['uciprint'],'MUTSIN',$i));
      }
      array_push($uciarr,"END FILES");
      array_push($uciarr," ");
      array_push($uciarr,"OPN SEQUENCE");
      array_push($uciarr,"    INGRP              INDELT $timestep");

      $uciarr = array_merge($uciarr,$openseq);

      array_push($uciarr,"    END INGRP");
      array_push($uciarr,"END OPN SEQUENCE");
      array_push($uciarr," ");
      array_push($uciarr,"MUTSIN");
      array_push($uciarr,"  MUTSINFO");
      array_push($uciarr,"*** MUTSIN  MFL  NPT  NMN  NLI MSFG");
      array_push($uciarr,"*** x -  x");

      $uciarr = array_merge($uciarr,$mutsinfo);
      #print_r($mutsinfo);

      array_push($uciarr,"  END MUTSINFO");
      array_push($uciarr,"");
      array_push($uciarr,"END MUTSIN");
      array_push($uciarr," ");
      array_push($uciarr,"EXT TARGETS");
      array_push($uciarr,"<-Volume-> <-Grp> <-Member-><--Mult-->Tran <-Volume-> <Member> Tsys Aggr Amd ***");
      array_push($uciarr,"<Name>   x        <Name> x x<-factor->strg <Name>   x <Name>qf  tem strg strg***");

      $uciarr = array_merge($uciarr,$exttargets);
      #print_r($exttargets);

      array_push($uciarr,"END EXT TARGETS");
      array_push($uciarr," ");
      array_push($uciarr,"END RUN");
      putArrayToFilePlatform("./$wdmfile$ucino.uci",$uciarr,1,'unix');
      array_push($ucistorun,"$wdmfile$ucino.uci");

      #print_r($uciarr);
    }


   putArrayToFilePlatform("./$wdmfile.exp",$impfile,1,'unix');

   # print a link to the file
   print("<br><a href='./$wdmfile.exp'>Import File</a>");
   print("<br><a href='./logs/$wdmfile$ucino.uci'>UCI File</a>");

   $wdimex = array();

   print_r($wdmfiles);

   if (in_array($wdmfile,$wdmfiles)) {

      /* do an import to existing wdm */
      array_push($wdimex,"$wdmfile");  /* send the wdm name to wdimex */
      array_push($wdimex,"$basepath/c.wdm");   /* indicate the wdm file which has header information */
      array_push($wdimex,"I");   /* select import option */
      array_push($wdimex,"$wdmfile.exp");   /* give the import file name */
      array_push($wdimex,"R");  /* return to operating system */

   } else {

      /* create the wdm, import the exp file, then run hspf and get the data */
      array_push($wdimex,"$wdmfile");  /* send the wdm name to wdimex */
      array_push($wdimex,"C");   /* select to create the WDM */
      array_push($wdimex,"N");   /* say no to putting headers on the file */
      array_push($wdimex,"$basepath/c.wdm");   /* indicate the wdm file which has header information */
      array_push($wdimex,"I");   /* select import option */
      array_push($wdimex,"$wdmfile.exp");   /* give the import file name */
      array_push($wdimex,"R");  /* return to operating system */

   }

   putArrayToFilePlatform("./$wdmfile.script",$wdimex,1,'unix');

   $wdimexresult = shell_exec("cat $wdmfile.script | /usr/local/bin/wdimex");
   #exec("cat $wdmfile.script | wdimex");
   if ($debug) {
      print("<br>Results of wdm creation ( cat $wdmfile.script | wdimex ): $wdimexresult <br>");
   }

   $hspfresult = shell_exec("chmod 664 $basepath/$wdmfile");
   print("<br>Results of: chmod 664 $basepath/$wdmfile: $hspfresult <br>");


   foreach ($ucistorun as $thisuci) {

      print("HSPF Command: echo $thisuci |/usr/local/bin/hspf<br>");
      $hspfresult = shell_exec("echo $thisuci |/usr/local/bin/hspf");
      print("<br>Results of hspf import to wdm: $hspfresult <br>");

   }

   #print_r($uciarr);

   #print_r($impfile);
}

print("<a href='1.ech'>HSPF Echo File</a>");


}

?>
