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
      real Tbnh4(maxlsegs),Tbno3(maxlsegs),Tblon(maxlsegs)  ! value of
      real Tbron(maxlsegs),Tbpo4(maxlsegs)                  ! target

      character*4 Tsnh4Name,Tsno3Name,TslonName  ! name of target
      data        Tsnh4Name,Tsno3Name,TslonName
     .            /'SNH4','SNO3','SLON'/
      character*4 TsronName,Tspo4Name
      data        TsronName,Tspo4Name
     .            /'SRON','SPO4'/
      character*4 Tbnh4Name,Tbno3Name,TblonName  ! name of target
      data        Tbnh4Name,Tbno3Name,TblonName
     .            /'BNH4','BNO3','BLON'/
      character*4 TbronName,Tbpo4Name
      data        TbronName,Tbpo4Name
     .            /'BRON','BPO4'       /

***************  EOF averages    values and names
      real Xdnh3(maxlsegs),Xsnh3(maxlsegs)   ! sorbed and surface
      real Xinh3(maxlsegs),Xanh3(maxlsegs)   ! interflow and gw
      real                 Xsno3(maxlsegs)
      real Xino3(maxlsegs),Xano3(maxlsegs)
      real Xdlon(maxlsegs),Xslon(maxlsegs)
      real Xilon(maxlsegs),Xalon(maxlsegs)
      real Xdron(maxlsegs),Xsron(maxlsegs)
      real Xiron(maxlsegs),Xaron(maxlsegs)
      real Xdpo4(maxlsegs),Xspo4(maxlsegs)
      real Xipo4(maxlsegs),Xapo4(maxlsegs)

      character*4 Xdnh3Name,Xsnh3Name,Xinh3Name,Xanh3Name
      data        Xdnh3Name,Xsnh3Name,Xinh3Name,Xanh3Name
     .           /'dnh3',   'snh3',   'inh3',   'anh3'/
      character*4           Xsno3Name,Xino3Name,Xano3Name
      data                  Xsno3Name,Xino3Name,Xano3Name
     .           /          'sno3',   'ino3',   'ano3'/
      character*4 XdlonName,XslonName,XilonName,XalonName
      data        XdlonName,XslonName,XilonName,XalonName
     .           /'dlon',   'slon',   'ilon',   'alon'/
      character*4 XdronName,XsronName,XironName,XaronName
      data        XdronName,XsronName,XironName,XaronName
     .           /'dron',   'sron',   'iron',   'aron'/
      character*4 Xdpo4Name,Xspo4Name,Xipo4Name,Xapo4Name
      data        Xdpo4Name,Xspo4Name,Xipo4Name,Xapo4Name
     .           /'dpo4',   'spo4',   'ipo4',   'apo4'/
      

******************* parameter values
      integer vcPOTFW,vcSQO,vcACQOP,vcSQOLIM
      integer vcWSQOP,vcIOQC,vcAOQC ! param file column

      real POTFW,SQO,ACQOP,SQOLIM,WSQOP,IOQC,AOQC    ! parameter value

********************** parameter update function
      real     newPOTFW,newSQO,newACQOP,newSQOLIM
      real     newWSQOP,newIOQC,newAOQC 
      external newPOTFW,newSQO,newACQOP,newSQOLIM
      external newWSQOP,newIOQC,newAOQC 
      
********************** parameter initialization function
      real     initialPOTFW,initialSQO,initialACQOP,initialSQOLIM
      real     initialWSQOP,initialIOQC,initialAOQC 
      external initialPOTFW,initialSQO,initialACQOP,initialSQOLIM
      external initialWSQOP,initialIOQC,initialAOQC 
      
