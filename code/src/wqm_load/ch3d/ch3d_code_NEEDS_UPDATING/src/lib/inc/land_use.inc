************************************************************************
***** PARAMETER FILE  -- changing this file changes the parameters in **
***** the entire pre- and post-processors ( recompile necessary )     **
*** lwat always means water and is a reserved land use name.  It      **
**  must always be the last land use listed
************************************************************************

      integer nlu
      integer lfor,lhvf,lpul,lpuh,lbar,lext,lpas,ltrp,lurs,lalf   ! perlnds
      integer lhyw,lhyo,lhom,lhwm,llwm   ! perlnds
      integer lnhy,lnpa,lnal,lnho,lnhi,lnlo   ! nutrient management perlnds
      integer lafo,liml,limh           ! implnds
      integer lwat                                                  ! water
********* parameters to size the problem

      parameter (nlu=25)                                 ! number of land uses
 
******** the following parameters correspond to nlu
      parameter (lfor =  1)
      parameter (lhvf =  2)
      parameter (lpul =  3)
      parameter (lpuh =  4)
      parameter (lbar =  5)
      parameter (lext =  6)
      parameter (lpas =  7)
      parameter (ltrp =  8)
      parameter (lurs =  9)
      parameter (lalf = 10)
      parameter (lhyw = 11)
      parameter (lhyo = 12)
      parameter (lhom = 13)
      parameter (lhwm = 14)
      parameter (llwm = 15)
      parameter (lnhy = 16)
      parameter (lnpa = 17)
      parameter (lnal = 18)
      parameter (lnho = 19)
      parameter (lnhi = 20)
      parameter (lnlo = 21)
      parameter (lafo = 22)
      parameter (liml = 23)
      parameter (limh = 24)
      parameter (lwat = 25)


      character*3 luname(nlu)

      data luname /'for','hvf','pul','puh','bar','ext','pas','trp',
     .             'urs','alf','hyw','hyo','hom','hwm','lwm','nhy',
     .             'npa','nal','nho','nhi','nlo','afo','iml','imh',
     .             'wat'/

**************** this could be replaced by a nlumax parameter and a data file
****************  that could be read whenever needed
      
