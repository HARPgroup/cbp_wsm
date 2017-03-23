************************************************************************
** get the factor by river and bvar for this scenario                 **
************************************************************************
      subroutine getRiverFactor(
     I                          modscen,rsegs,maxlrsegs,nrsegs,
     I                          nBvar,Bname,
     O                          RiverFactor)
      implicit none
      include '../wqmlib/transfer_wdm.inc'

      integer maxlrsegs

      character*(*) modscen  ! scenario for this river
      integer lenmodscen

      integer nBvarForColumn(maxBvar*2)  ! can be extraneous columns
                                     ! same file for ps and nps

********** utilities
      character*300 longline

      character*4 TBname  ! reading temp variable
      character*13 Trseg,ttrseg

      integer nq,nr,nc  ! indices
      integer ncols     ! number of columns in data file

      logical foundseg(maxlrsegs),found

      character*13 LRsegsRsegs(maxlrsegs)

      real RiverFactor(maxlrsegs,0:maxBvar) ! extra column for scratch

************* river factor for scenarios that are a percentage of other
***********  scenarios, useful for running geographic isolation runs

      do nr = 1,nrsegs
        print*,'in getr ',nr,' ',rsegs(nr)
      end do

      nc = nrsegs
*********** initialize, check for negatives later
      do nq = 1,nBvar
        do nr = 1,nrsegs
          RiverFactor(nr,nq) = -999.0
          if (nc.ne.nrsegs) print*,'aaa ',nq,nr,nc,nrsegs
        end do
      end do
      do nr = 1,nrsegs
        foundseg(nr) = .false.
        if (nc.ne.nrsegs) print*,'bbb ',nr
      end do
    
************ open file
      call lencl(modscen,lenmodscen)
      fnam= ScenDatDir//'wqm/RiverFactors/'//modscen(:lenmodscen)//
     .      '/riverfactors.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

********* find the bvar number for each column
      read(dfile,'(a300)',err=992,end=992) longline
      call d2x(longline,last)
      if (longline(300-3:300).ne.'    ') go to 993
      call shift(longline)  ! get rid of first column

      ncols = 0
      do ! parse line
        if (longline(:10).eq.'          ') exit
        read(longline,*,err=992,end=992) TBname
        ncols = ncols + 1
        nBvarForColumn(ncols) = 0
        do nq = 1,nBvar
          if (TBname.eq.Bname(nq)) nBvarForColumn(ncols) = nq
        end do
        call shift(longline)
      end do

****************** read and process nrsegs, populate RiverFactor
      do 
        read(dfile,'(a300)',err=992,end=111) longline     ! read a line
        call d2x(longline,last)
        if (longline(300-3:300).ne.'    ') go to 993

        read(longline,*) Trseg
        do nr = 1,nrsegs
          if (Trseg.eq.rsegs(nr)) then
            foundseg(nr) = .true.
            read(longline,*,err=995,end=996) 
     .       ttrseg,(RiverFactor(nr,nBvarForColumn(nc)),nc=1,ncols)
          end if
        end do
      end do
111   close(dfile)

      do nr = 1,nrsegs
        if (.not.foundseg(nr)) go to 994
      end do

      return

********************************* ERROR SPACE **************************
991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading following file near line'
      report(2) = fnam
      report(3) = longline
      go to 999

993   report(1) = 'Problem reading following file:  line too long:'
      report(2) = fnam
      report(3) = longline
      go to 999

994   report(1) = 'did not find seg in file'
      report(2) = fnam
      report(3) = rsegs(nr)
      go to 999

995   report(1) = 'unspecified error parsing line: in file:'
      report(2) = longline
      report(3) = fnam
      go to 999

996   report(1) = 'error parsing line: in file: too few columns'
      report(2) = longline
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end
