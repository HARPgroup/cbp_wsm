************************************************************************
** program to change parameters for iqual calibration.                **
**   reads compares outputs to targets and modifies the parameters    **
*************************************************************************
      implicit none
      include 'iqual.inc'

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
     
      module = 'IQUAL'
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

************ READ IN SIMULATED EOF
      call getsimEOFnv(
     I               Xdnh3Name,clu,lscen,lsegs,nlsegs,
     O               Xdnh3)
      call getsimEOFnv(
     I               Xsnh3Name,clu,lscen,lsegs,nlsegs,
     O               Xsnh3)
      call getsimEOFnv(
     I               Xsno3Name,clu,lscen,lsegs,nlsegs,
     O               Xsno3)
      call getsimEOFnv(
     I               XdlonName,clu,lscen,lsegs,nlsegs,
     O               Xdlon)
      call getsimEOFnv(
     I               XslonName,clu,lscen,lsegs,nlsegs,
     O               Xslon)
      call getsimEOFnv(
     I               XdronName,clu,lscen,lsegs,nlsegs,
     O               Xdron)
      call getsimEOFnv(
     I               XsronName,clu,lscen,lsegs,nlsegs,
     O               Xsron)
      call getsimEOFnv(
     I               Xdpo4Name,clu,lscen,lsegs,nlsegs,
     O               Xdpo4)
      call getsimEOFnv(
     I               Xspo4Name,clu,lscen,lsegs,nlsegs,
     O               Xspo4)

      print*,'all accounting'

************** REWRITE THE BASIN FILE SO THAT YOU ONLY RUN
************ UNCALIBRATED SEGMENTS
      nuncalsegs = 0
      ncalsegs = 0
      do ns = 1,nlsegs
        if (.not.calibrated(
     I                      Tsnh4(ns),Tsno3(ns),Tslon(ns),Tsron(ns),
     I                      Tspo4(ns),
     I                      Xdnh3(ns),Xsnh3(ns),Xsno3(ns),Xdlon(ns),
     I                      Xslon(ns),Xdron(ns),Xsron(ns),Xdpo4(ns),
     I                      Xspo4(ns))) then
          nuncalsegs = nuncalsegs + 1
          uncalsegs(nuncalsegs) = lsegs(ns)
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

