************************************************************************
** program to change parameters for pqual calibration.                **
**   reads compares outputs to targets and modifies the parameters    **
*************************************************************************
      implicit none
      include 'pqual.inc'

      integer ns
      character*12 module
      integer lenmod

      character*25 targscen

      integer nlinemax, nlines  ! # of lines in parameter files
      parameter (nlinemax=400)
      character*2000 parline(nlinemax),
     .                       varline,tableline,
     .                       vkeep,tkeep
      integer i,j,nl,nc       ! indices
      integer iline
      character*4 version ! version number

      logical found
      logical fileexist  ! if file already exists

      logical calibrated ! true if segment is calibrated
      external calibrated
      character*6 uncalsegs(maxlsegs)  ! segments still not calibrated
      integer nuncalsegs
      character*6 calsegs(maxlsegs)  ! segments still not calibrated
      integer ncalsegs
      logical calib(maxlsegs)  ! is this segment calibrated?

      character*100 basin  ! name of seglist file
      character*1 cnq  ! character number of quality consituent
      character*3 clu  ! character land use
   
*************** END DECLARATIONS ***************************************

************ read in specifications
      read*,calscen,lscen,basin,clu
      call lencl(lscen,lenlscen)
      
      call readcontrol_Lparamscen(
     I                            lscen,lenlscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)
     
      module = 'PQUAL'
      call uppercase(module)
      call lencl(module,lenmod)

************ GET LAND SEGMENTS 
      call readLandSeglist(
     I                     basin,
     O                     lsegs,nlsegs)
    
******* READ IN TARGETS
      call gettargscen(
     I                 calscen,
     O                 targscen)
      call gettarget(
     I               targscen,Tsnh4Name,lsegs,nlsegs,clu,
     O               Tsnh4)
      call gettarget(
     I               targscen,Tsno3Name,lsegs,nlsegs,clu,
     O               Tsno3)
      call gettarget(
     I               targscen,TslonName,lsegs,nlsegs,clu,
     O               Tslon)
      call gettarget(
     I               targscen,TsronName,lsegs,nlsegs,clu,
     O               Tsron)
      call gettarget(
     I               targscen,Tspo4Name,lsegs,nlsegs,clu,
     O               Tspo4)
      call gettarget(
     I               targscen,Tbnh4Name,lsegs,nlsegs,clu,
     O               Tbnh4)
      call gettarget(
     I               targscen,Tbno3Name,lsegs,nlsegs,clu,
     O               Tbno3)
      call gettarget(
     I               targscen,TblonName,lsegs,nlsegs,clu,
     O               Tblon)
      call gettarget(
     I               targscen,TbronName,lsegs,nlsegs,clu,
     O               Tbron)
      call gettarget(
     I               targscen,Tbpo4Name,lsegs,nlsegs,clu,
     O               Tbpo4)
C       print*,'Targets'
C       print*,Tsnh4(1),Tsno3(1),Tslon(1),Tsron(1),Tspo4(1)
C       print*,Tbnh4(1),Tbno3(1),Tblon(1),Tbron(1),Tbpo4(1)                  


************ READ IN SIMULATED EOF
      call getsimEOFnv(
     I               Xdnh3Name,clu,lscen,lsegs,nlsegs,
     O               Xdnh3)
      call getsimEOFnv(
     I               Xsnh3Name,clu,lscen,lsegs,nlsegs,
     O               Xsnh3)
      call getsimEOFnv(
     I               Xinh3Name,clu,lscen,lsegs,nlsegs,
     O               Xinh3)
      call getsimEOFnv(
     I               Xanh3Name,clu,lscen,lsegs,nlsegs,
     O               Xanh3)
      call getsimEOFnv(
     I               Xsno3Name,clu,lscen,lsegs,nlsegs,
     O               Xsno3)
      call getsimEOFnv(
     I               Xino3Name,clu,lscen,lsegs,nlsegs,
     O               Xino3)
      call getsimEOFnv(
     I               Xano3Name,clu,lscen,lsegs,nlsegs,
     O               Xano3)
      call getsimEOFnv(
     I               XdlonName,clu,lscen,lsegs,nlsegs,
     O               Xdlon)
      call getsimEOFnv(
     I               XslonName,clu,lscen,lsegs,nlsegs,
     O               Xslon)
      call getsimEOFnv(
     I               XilonName,clu,lscen,lsegs,nlsegs,
     O               Xilon)
      call getsimEOFnv(
     I               XalonName,clu,lscen,lsegs,nlsegs,
     O               Xalon)
      call getsimEOFnv(
     I               XdronName,clu,lscen,lsegs,nlsegs,
     O               Xdron)
      call getsimEOFnv(
     I               XsronName,clu,lscen,lsegs,nlsegs,
     O               Xsron)
      call getsimEOFnv(
     I               XironName,clu,lscen,lsegs,nlsegs,
     O               Xiron)
      call getsimEOFnv(
     I               XaronName,clu,lscen,lsegs,nlsegs,
     O               Xaron)
      call getsimEOFnv(
     I               Xdpo4Name,clu,lscen,lsegs,nlsegs,
     O               Xdpo4)
      call getsimEOFnv(
     I               Xspo4Name,clu,lscen,lsegs,nlsegs,
     O               Xspo4)
      call getsimEOFnv(
     I               Xipo4Name,clu,lscen,lsegs,nlsegs,
     O               Xipo4)
      call getsimEOFnv(
     I               Xapo4Name,clu,lscen,lsegs,nlsegs,
     O               Xapo4)
C       print*,'loads'
C       print*,Xdnh3(1),Xsnh3(1),Xinh3(1),Xanh3(1),Xsno3(1),Xino3(1)
C       print*,Xano3(1),Xdlon(1),Xslon(1),Xilon(1),Xalon(1),Xdron(1)
C       print*,Xsron(1),Xiron(1),Xaron(1),Xdpo4(1),Xspo4(1),Xipo4(1)
C       print*,Xapo4(1)


      print*,'all accounting'

************** REWRITE THE BASIN FILE SO THAT YOU ONLY RUN
************ UNCALIBRATED SEGMENTS
      nuncalsegs = 0
      ncalsegs = 0
      do ns = 1,nlsegs
        calib(ns) = .true.
        if (.not.calibrated(
     I      Tsnh4(ns),Tsno3(ns),Tslon(ns),Tsron(ns),Tspo4(ns),
     I      Tbnh4(ns),Tbno3(ns),Tblon(ns),Tbron(ns),Tbpo4(ns), 
     I      Xdnh3(ns),Xsnh3(ns),Xinh3(ns),Xanh3(ns),Xsno3(ns),
     I      Xino3(ns),Xano3(ns),Xdlon(ns),Xslon(ns),Xilon(ns),
     I      Xalon(ns),Xdron(ns),Xsron(ns),Xiron(ns),Xaron(ns),
     I      Xdpo4(ns),Xspo4(ns),Xipo4(ns),Xapo4(ns))) then
          nuncalsegs = nuncalsegs + 1
          uncalsegs(nuncalsegs) = lsegs(ns)
          calib(ns) = .false.
        else
          ncalsegs = ncalsegs + 1
          calsegs(ncalsegs) = lsegs(ns)
        end if
      end do

      write(*,*) 'CALIBRATED SEGMENTS'
      do ns = 1,ncalsegs
        write(*,*) calsegs(ns)
      end do
      write(*,*) 'UNCALIBRATED SEGMENTS'
      do ns = 1,nuncalsegs
        write(*,*) uncalsegs(ns)
      end do
      write(*,*) 'There are ',ncalsegs,' calibrated ',clu,' segments'
      write(*,*)'There are ',nuncalsegs,' uncalibrated ',clu,' segments'

      end

