************************************************************************
** Program to create generate weights between river segments and      **
**   calibration sites.  The sum of all calibration-site weights for  **
**   each segment must equal 1                                        **
** NOTE, nrsegs in this program means the number of segments that are **
**   in the calibration, not the total in the model as in most        **
**   programs.  other routines are slightly different as well.        **
************************************************************************
      include 'calib_site.inc'

      integer i  ! index

      character*25 calscen,caltype,basin,obscen
      integer lencalscen,lencaltype,lenbasin,lenobscen

      integer nc1,nc2,nr1,nr2,itemp,ncsegs2,nr

      integer npoints,nonLOD  ! number of data points found

      integer year1,year2,ny

      logical foundall,anyobs(maxrsegs)
      integer upstream(maxrsegs),nup

      integer nm,nd,nh,nmin   ! variables to read observed file
      real value
      character*1 qflag

************ END DECLARATIONS ******************************************
      print*,' determining the weights for use in calibration'

      read*,rscen,calscen,caltype,obscen,basin,year1,year2

      call lencl(rscen,lenrscen)
      call lencl(calscen,lencalscen)
      call lencl(caltype,lencaltype)
      call lencl(basin,lenbasin)
      call lencl(obscen,lenobscen)
      if (lenbasin.eq.25) go to 998

*********** read in all possible calibration segments
********** and store list of simulated rivers
******* csegs and rsegs will differ in that csegs will contain doubles
      call readRiverSeglist(
     I                      basin,
     O                      rsegs,nrsegs)
      do nr = 1,nrsegs
        csegs(nr) = rsegs(nr)
      end do
      ncsegs = nrsegs

*********** populate uniqid, dsid, uniqindex
      do ns = 1,supermax
        uniqindex(ns) = 0
      end do
      do nr = 1,nrsegs
        read(rsegs(nr)(5:8),'(i4)') uniqid(nr)
        read(rsegs(nr)(10:13),'(i4)') dsid(nr)
        uniqindex(uniqid(nr)) = nr
      end do

*********** read in possible doubles as well
************* add to the end of csegs
      call adddoubles(
     I                rscen,lenrscen,
     M                csegs,ncsegs)
************* got all csegs, ncsegs

************ CHECK TO FIND SEGS THAT ARE CALIBRATION SEGS
********* get possible concentrations
      call concinfo(
     I              rscen,lenrscen,
     O              nconcs,concname)

******** loop over simulated segments and concentration types
********** keep those segments that have data
C      print*,'finding observed values for '
      do ns = 1,ncsegs

C        print*,csegs(ns)

        do nc = 1,nconcs
          if (concname(nc).eq.'FLOW') cycle
          npoints = 0
          nonLOD = 0

          fnam = tree//'input/calib/observed/'//obscen(:lenobscen)//'/'
     .           //concname(nc)//'/'//csegs(ns)//'.O'//concname(nc)
          open (dfile,file=fnam,status='old',iostat=err)
          if (err.eq.0) then
            do
              read(dfile,*,end=333) ny,nm,nd,nh,nmin,value,qflag
              if (ny.ge.year1 .and. ny.le.year2) then
                npoints = npoints + 1
                if (qflag.ne.'<') nonLOD = nonLOD + 1
              end if
            end do
333         close(dfile)           ! close observed load file
          end if

          obs(ns,nc) = npoints
          goodobs(ns,nc) = nonLOD
        end do

        anyobs(ns) = .false.
        do nc = 1,nconcs
          if (obs(ns,nc).ne.0) anyobs(ns) = .true.
        end do

      end do

*********** remove segs from ncsegs that do not have data
      ncsegs2 = ncsegs
      do ns = 1,ncsegs2     ! get rid of missing values
        if (ns.gt.ncsegs) exit
        if (.not.anyobs(ns)) then
          do while (.not.anyobs(ncsegs))
            ncsegs = ncsegs - 1
            if (ncsegs.eq.0) exit
          end do
          if (ncsegs.lt.ns) exit
          csegs(ns) = csegs(ncsegs)
          anyobs(ns) = anyobs(ncsegs)
          do nc = 1,nconcs
            obs(ns,nc) = obs(ncsegs,nc)
            goodobs(ns,nc) = goodobs(ncsegs,nc)
          end do
          csegs(ncsegs) = ' '
          ncsegs = ncsegs - 1
        end if
      end do

******** find list of all upstream segments for each station
      do ns = 1,ncsegs  
        call findupstream(
     I                    csegs(ns),
     I                    nrsegs,uniqindex,uniqid,dsid,
     O                    upstream,nup)
        nallup(ns) = nup
        do nr = 1,nup
          allup(ns,nr) = upstream(nr)
        end do
        if (csegs(ns)(10:13).eq.'0003') then
          nallup(ns) = nup-1
        end if
      end do  

********* get area for each rseg
      do nr = 1,nrsegs
        call getarea(
     I               rsegs(nr),rscen,lenrscen,
     O               rsegarea(nr))
C        print*,'area ',rsegs(nr),' ',rsegarea(nr)
      end do

************** determine weight for each rseg/cseg/conc trio
      call getweight(
     I               maxrsegs,maxcsegs,nconcmax,
     I               rsegs,nrsegs,csegs,ncsegs,nconcs,
     I               allup,nallup,
     I               obs,goodobs,rsegarea,
     O               weight)

      fnam = tree//'config/control/calib/'//caltype(:lencaltype)//'/'
     .       //calscen(:lencalscen)//'/weights/'//basin(:lenbasin)//
     .       '_weights.bin'
      open(dfile,file=fnam,form='unformatted',
     .           status='unknown',iostat=err)
      if (err.ne.0) go to 991
      write(dfile) rsegs,csegs,nrsegs,ncsegs,goodobs,weight,checkend
      close(dfile)

      stop

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

998   report(1) = 'Problem with basin '//basin
      report(2) = ' rename with fewer characters, or change code:'
      report(3)=' ./code/src/calibration_utils/make_WQ_calib_site_list/'
      go to 999

999   call stopreport(report)

      end

