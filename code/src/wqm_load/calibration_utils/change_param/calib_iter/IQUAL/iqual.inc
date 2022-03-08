************************************************************************
** variables used frequently in the calibration parameter change prog **
************************************************************************
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include '../../../../lib/inc/lsegs.inc'
      include '../../../../lib/inc/land_use.inc'
      include 'piqual.inc'

      character*25 calscen    ! set of specifications for this
      integer lencalscen      ! calibration run
      integer columnorder(nlu)  ! order to read table

************ Targets
      real Tsnh4(maxlsegs),Tsno3(maxlsegs),Tslon(maxlsegs)  ! value of
      real Tsron(maxlsegs),Tspo4(maxlsegs)                  ! target

      character*4 Tsnh4Name,Tsno3Name,TslonName  ! name of target
      data        Tsnh4Name,Tsno3Name,TslonName
     .            /'SNH4','SNO3','SLON'/
      character*4 TsronName,Tspo4Name
      data        TsronName,Tspo4Name
     .            /'SRON','SPO4'/

***************  EOF averages    values and names
      real Xdnh3(maxlsegs),Xsnh3(maxlsegs),Xsno3(maxlsegs) 
      real Xdlon(maxlsegs),Xslon(maxlsegs),Xdron(maxlsegs)
      real Xsron(maxlsegs),Xdpo4(maxlsegs),Xspo4(maxlsegs)

      character*4 Xdnh3Name,Xsnh3Name,Xsno3Name
      data        Xdnh3Name,Xsnh3Name,Xsno3Name
     .           /'dnh3','snh3','sno3'/
      character*4 XdlonName,XslonName,XdronName,XsronName
      data        XdlonName,XslonName,XdronName,XsronName
     .           /'dlon','slon','dron','sron'/
      character*4 Xdpo4Name,Xspo4Name
      data        Xdpo4Name,Xspo4Name
     .           /'dpo4','spo4'/
      

******************* parameter values
      integer vcPOTFW,vcSQO,vcACQOP,vcSQOLIM,vcWSQOP ! param file column

      real POTFW,SQO,ACQOP,SQOLIM,WSQOP    ! parameter value

      real     newPOTFW,newSQO,newACQOP,newSQOLIM,newWSQOP  ! update 
      external newPOTFW,newSQO,newACQOP,newSQOLIM,newWSQOP  ! function
      real     initialPOTFW,initialSQO,initialACQOP
      real     initialSQOLIM,initialWSQOP                   ! initial
      external initialPOTFW,initialSQO,initialACQOP
      external initialSQOLIM,initialWSQOP                   ! function
      
