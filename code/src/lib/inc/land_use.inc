************************************************************************
***** PARAMETER FILE  -- changing this file changes the parameters in **
***** the entire pre- and post-processors ( recompile necessary )     **
*** lwat always means water and is a reserved land use name.  It      **
**  must always be the last land use listed
************************************************************************

      integer nlu
      integer lfor,lhvf,lpas,ltrp,lurs,lalf            ! perlnds
      integer lhyw,lhyo,lhom,lhwm,llwm                 ! perlnds
      integer lnhy,lnpa,lnal,lnho,lnhi,lnlo            ! nutrient management perlnds
      integer lrpd,lnpd,lcpd,lrcn,lccn,lrex,lnex,lcex  !perlnds

      integer lafo,lcfo,lrid,lnid,lcid                 ! implnds
      integer lwat                                     ! water

********* parameters to size the problem
      parameter (nlu=31)                               ! number of land uses
 
******** the following parameters correspond to nlu
      parameter (lfor =  1)
      parameter (lhvf =  2)
      parameter (lhwm =  3)
      parameter (llwm =  4)
      parameter (lhom =  5)
      parameter (lalf =  6)
      parameter (lhyw =  7)
      parameter (lhyo =  8)
      parameter (lpas =  9)
      parameter (ltrp = 10)
      parameter (lurs = 11)
      parameter (lnhi = 12)
      parameter (lnlo = 13)
      parameter (lnho = 14)
      parameter (lnal = 15)
      parameter (lnhy = 16)
      parameter (lnpa = 17)
      parameter (lrpd = 18)
      parameter (lnpd = 19)
      parameter (lcpd = 20)
      parameter (lrcn = 21)
      parameter (lccn = 22)
      parameter (lrex = 23)
      parameter (lnex = 24)
      parameter (lcex = 25)
      parameter (lafo = 26)
      parameter (lcfo = 27)
      parameter (lrid = 28)
      parameter (lnid = 29)
      parameter (lcid = 30)
      parameter (lwat = 31)

      character*3 luname(nlu)

      data luname /'for','hvf','hwm','lwm','hom','alf','hyw','hyo',
     .             'pas','trp','urs','nhi','nlo','nho','nal','nhy',
     .             'npa','rpd','npd','cpd','rcn','ccn','rex','nex',
     .             'cex','afo','cfo','rid','nid','cid','wat'/

**************** this could be replaced by a nlumax parameter and a data file
****************  that could be read whenever needed
      
