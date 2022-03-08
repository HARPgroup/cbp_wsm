************************************************************************
***** PARAMETER FILE  -- changing this file changes the parameters in **
***** the entire pre- and post-processors ( recompile necessary )     **
*** lwat always means water and is a reserved land use name.  It      **
**  must always be the last land use listed
************************************************************************

      integer nlu

      integer laop,lsoy,lgwm,lgom,llhy,lswm,lsom,lsgg,ldbl,lsch
      integer lscl,loac,lohy,lpas,lnch,lntg,lmch,lmtg,lmcn,lcch
      integer lctg,lccn,lcmo,lcfr,lhfr,lfor,lwfp,lwto,losp

      integer lfsp,lfnp,lnir,lnnr,lnci,lmir,lmnr,lmci,lcir,lcnr
      integer lcci,lstb,lstf,lsho

      integer lwat

********* parameters to size the problem
      parameter (nlu=44)                               ! number of land uses
 
******** the following parameters correspond to nlu
      parameter (laop =    1)
      parameter (lsoy =    2)
      parameter (lgwm =    3)
      parameter (lgom =    4)
      parameter (llhy =    5)
      parameter (lswm =    6)
      parameter (lsom =    7)
      parameter (lsgg =    8)
      parameter (ldbl =    9)
      parameter (lsch =    10)
      parameter (lscl =    11)
      parameter (loac =    12)
      parameter (lohy =    13)
      parameter (lpas =    14)
      parameter (lnch =    15)
      parameter (lntg =    16)
      parameter (lmch =    17)
      parameter (lmtg =    18)
      parameter (lmcn =    19)
      parameter (lcch =    20)
      parameter (lctg =    21)
      parameter (lccn =    22)
      parameter (lcmo =    23)
      parameter (lcfr =    24)
      parameter (lhfr =    25)
      parameter (lfor =    26)
      parameter (lwfp =    27)
      parameter (lwto =    28)
      parameter (losp =    29)
      parameter (lfsp =    30)
      parameter (lfnp =    31)
      parameter (lnir =    32)
      parameter (lnnr =    33)
      parameter (lnci =    34)
      parameter (lmir =    35)
      parameter (lmnr =    36)
      parameter (lmci =    37)
      parameter (lcir =    38)
      parameter (lcnr =    39)
      parameter (lcci =    40)
      parameter (lstb =    41)
      parameter (lstf =    42)
      parameter (lsho =    43)
      parameter (lwat =    44)

      character*3 luname(nlu)

      data luname /'aop','soy','gwm','gom','lhy','swm','som','sgg',
     . 'dbl','sch','scl','oac','ohy','pas','nch','ntg','mch','mtg',
     . 'mcn','cch','ctg','ccn','cmo','cfr','hfr','for','wfp','wto',
     . 'osp','fsp','fnp','nir','nnr','nci','mir','mnr','mci','cir',
     . 'cnr','cci','stb','stf','sho','wat'/

**************** this could be replaced by a nlumax parameter and a data file
****************  that could be read whenever needed
      
