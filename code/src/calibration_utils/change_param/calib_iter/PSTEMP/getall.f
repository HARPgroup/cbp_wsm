************************************************************************
** subroutines to read all of the data, share common blocks with main **
************************************************************************

************************************************************************
** subroutine get river data                                          **
************************************************************************
      subroutine getriver(module,lenmod,calscen,lencalscen,
     O           rsegs,uniqindex,uniqid,dsid,nrsegs,fnam,err)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include '../../../../lib/inc/rsegs.inc'
      logical comment
      external comment
      integer ic
      character*(*) module
      character*(*) calscen    ! set of specifications for this
      integer lencalscen,lenmod      ! calibration run

      nrsegs = 0
C      print*,'getting river data'
      fnam = controldir//'calib/'//module(:lenmod)//'/'//
     .       calscen(:lencalscen)//'/rsegs.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) then 
        err = 991
        return
      end if

      read(dfile,'(a100)',err=992,end=993)line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then
          call findcomma(line,ic)
          Tseg = line(:ic-1)
          call trims(Tseg,last)
          if (last.ne.13) then 
            err = 992
            return
          end if
          nrsegs = nrsegs + 1
          read(Tseg(5:8),'(i4)')uniqid(nrsegs)
          read(Tseg(10:13),'(i4)')dsid(nrsegs)
          rsegs(nrsegs) = Tseg
          uniqindex(uniqid(nrsegs))=nrsegs
        end if
        read(dfile,'(a100)',err=992,end=993)line
        call d2x(line,last)
      end do
      close (dfile)
      return

992   err = 992
      return
993   err = 993
      return

      end

************************************************************************
** subroutine get land data                                           **
************************************************************************
      subroutine getland(module,lenmod,calscen,lencalscen,
     O                   lsegs,nlsegs,fnam,err)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include '../../../../lib/inc/lsegs.inc'
      logical comment
      external comment
      integer ic
      character*(*) module
      character*(*) calscen    ! set of specifications for this
      integer lencalscen,lenmod      ! calibration run

      nlsegs = 0
C      print*,'getting land data'
      fnam = controldir//'calib/'//module(:lenmod)//'/'//
     .       calscen(:lencalscen)//'/lsegs.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) then 
        err = 991
        return
      end if

      read(dfile,'(a100)',err=992,end=993)line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then
          call findcomma(line,ic)
          Tseg = line(:ic-1)
          call trims(Tseg,last)
          if (last.ne.6) then 
            err = 993
            return
          end if
          nlsegs = nlsegs + 1
          lsegs(nlsegs) = Tseg
        end if
        read(dfile,'(a100)',err=992,end=993)line
        call d2x(line,last)
      end do
      close (dfile)
      return

992   err = 992
      return
993   err = 993
      return

      end


************************************************************************
** subroutine get orphans and surrogates                              **
************************************************************************
      subroutine getorphans(module,lenmod,calscen,lencalscen,
     O                      orphans,surrogates,norphans,fnam,err)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'tempcal.inc'
      logical comment
      external comment
      integer ic

      norphans = 0
      fnam = controldir//'calib/'//module(:lenmod)//'/'//
     .       calscen(:lencalscen)//'/'//calscen(:lencalscen)//
     .       '_orphans.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) then
        err = 991
        return
      end if

      read(dfile,'(a100)',err=992,end=993)line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then
          call findcomma(line,ic)
          Tseg = line(:ic-1)
          call trims(Tseg,last)
          if (last.ne.6) then
            err = 993
            return
          end if
          call shift(line)
          call trims(line,last)
          if (last.ne.6) then
            err = 993
            return
          end if
          norphans = norphans + 1
          orphans(norphans) = Tseg
          surrogates(norphans) = line(:last)
        end if
        read(dfile,'(a100)',err=992,end=993)line
        call d2x(line,last)
      end do
      close (dfile)
      return

992   err = 992
      return
993   err = 993
      return

      end


************************************************************************
** subroutine to get river weights for each land segment              **
************************************************************************
      subroutine getR2L(module,lenmod,calscen,lencalscen,
     I                  lsegs,nlsegs,uniqindex,
     O                  weight,R2L,nR2L,fnam,err)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'tempcal.inc'
      logical comment
      external comment
      integer ic,nr
      real test

      logical found

************ END DECLARATION *******************************************
      do nl = 1,nlsegs
        nR2L(nl) = 0
      end do
      do nl = 1,nlsegs
        do nr = 1,maxR2L
          R2L(nl,nr) = 0
          weight(nl,nr) = 0.0
        end do
      end do
      
      fnam = controldir//'calib/'//module(:lenmod)//'/'//
     .       calscen(:lencalscen)//'/'//calscen(:lencalscen)//
     .       '_river_land.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) then
        err = 991
        return
      end if

      read(dfile,'(a100)',err=992,end=993)line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then

          call findcomma(line,ic)  ! get river
          Tseg = line(:ic-1)
          call trims(Tseg,last)
          if (last.ne.13) then
            err = 994
            return
          end if
          read(Tseg(5:8),'(i4)',err=994) nr
          nr = uniqindex(nr)

          call shift(line)      ! get land
          call findcomma(line,ic)
          Tseg = line(:ic-1)
          call trims(Tseg,last)
          if (last.ne.6) then
            err = 995
            return
          end if
          found = .false.
          do nl = 1,nlsegs
            if (Tseg(:6).eq.lsegs(nl)) then
              found = .true.
              exit
            end if
          end do
          if (.not.found) go to 995

          call shift(line)  ! get weight
          nR2L(nl) = nR2L(nl) + 1
          R2L(nl,nR2L(nl)) = nr
          read(line,*,err=997,end=997) weight(nl,nR2L(nl))
        end if
        read(dfile,'(a100)',err=992,end=993)line
        call d2x(line,last)
      end do
      close (dfile)

      do nl = 1,nlsegs  ! test for weight
        if (nR2L(nl).ne.0) then
          test = 0.0
          do nr = 1,nR2L(nl)
            test = test + weight(nl,nr)
          end do
          if (abs(test-1.0).gt.0.01) go to 996
        end if
      end do

      return

************** ERROR SPACE *****************************************
992   err = 992
      return
993   err = 993
      return
994   err = 994
      return
995   err = 995
      return
996   err = 996
      print*,lsegs(nl)
      return
997   err = 997
      print*,rsegs(nr),lsegs(nl)
      return

      end


************************************************************************
** subroutine get river calibration stats                             **
************************************************************************
      subroutine getRstats(
     I               rscen,uniqindex,version,R2L,nR2L,nlsegs,rsegs,
     O               facASLT,facULTP1,facLGTP1)
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'tempcal.inc'
      real rdum
      integer nr,nm
      character*(*) version
      character*300 statline

      integer nm2
      
************ Calibration Parameters
      real SFTbias(maxrsegs,12)  ! storm flow temperature bias
      real BFTbias(maxrsegs,12)  ! base flow temperature bias

************* functions to calculate factors
      real calfacASLT,calfacULTP1,calfacLGTP1
      external calfacASLT,calfacULTP1,calfacLGTP1

      integer surrogate
      logical missing
      external missing


      do nr = 1,maxrsegs    ! set to -9 to test that calib data exists
        facASLT(nr,1) = -9.0
      end do

      call lencl(rscen,lenrscen)
      fnam = outdir//'river/summary/'//rscen(:lenrscen)//
     .             '_wtmp_stats_'//version//'.csv'
      open(dfile+1,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      fnam = outdir//'river/summary/'//rscen(:lenrscen)//
     .             '_wtmp_stats.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a300)',end=100,err=992) statline
      call ryt(statline,dfile+1)
      do
        read(dfile,'(a300)',end=100,err=992) statline
        call ryt(statline,dfile+1)
        read(statline(5:8),'(i4)',err=992,end=992) nr
        nr = uniqindex(nr)
        call shift(statline)
        read(statline,*,err=992,end=992)
     .            (SFTbias(nr,nm),nm=1,12),(BFTbias(nr,nm),nm=1,12)


************ if missing value look for close surrogate
*********** try the same month in quick/base flow first
        do nm = 1,12
          if (missing(SFTbias(nr,nm))) then
            if (.not.missing(BFTbias(nr,nm))) then
              SFTbias(nr,nm) = BFTbias(nr,nm)
            else 
              diff = 12
              surrogate = 0
              do nm2 = 1,12
                if (.not.missing(SFTbias(nr,nm2))
     .                .and.abs(nm2-nm).lt.diff) then
                  diff = nm2
                  surrogate = nm2
                end if
              end do
              if (surrogate.ne.0) then
                SFTbias(nr,nm) = SFTbias(nr,surrogate)
              else
                do nm2 = 1,12
                  if (.not.missing(BFTbias(nr,nm2))
     .                  .and.abs(nm2-nm).lt.diff) then
                    diff = nm2
                    surrogate = nm2
                  end if
                end do
                SFTbias(nr,nm) = BFTbias(nr,surrogate)
              end if
            end if
          end if
        end do
        do nm = 1,12
          if (missing(BFTbias(nr,nm))) then
            if (.not.missing(SFTbias(nr,nm))) then
              BFTbias(nr,nm) = SFTbias(nr,nm)
            else 
              diff = 12
              surrogate = 0
              do nm2 = 1,12
                if (.not.missing(BFTbias(nr,nm2))
     .                .and.abs(nm2-nm).lt.diff) then
                  diff = nm2
                  surrogate = nm2
                end if
              end do
              if (surrogate.ne.0) then
                BFTbias(nr,nm) = BFTbias(nr,surrogate)
              else
                do nm2 = 1,12
                  if (.not.missing(SFTbias(nr,nm2))
     .                  .and.abs(nm2-nm).lt.diff) then
                    diff = nm2
                    surrogate = nm2
                  end if
                end do
                BFTbias(nr,nm) = SFTbias(nr,surrogate)
              end if
            end if
          end if
        end do
               
        do nm = 1,12
          lastnm = nm - 1
          if (lastnm.eq.0) lastnm = 12
          facASLT(nr,nm) = calfacASLT(
     I                     SFTbias(nr,lastnm),SFTbias(nr,nm),
     I                     BFTbias(nr,lastnm),BFTbias(nr,nm))
          facULTP1(nr,nm) = calfacULTP1(
     I                      SFTbias(nr,lastnm),SFTbias(nr,nm),
     I                      BFTbias(nr,lastnm),BFTbias(nr,nm))
          facLGTP1(nr,nm) = calfacLGTP1(
     I                      SFTbias(nr,lastnm),SFTbias(nr,nm),
     I                      BFTbias(nr,lastnm),BFTbias(nr,nm))
        end do

      end do

100   close(dfile)
      close(dfile+1)

************** CHECK THAT THE RIVER SEGMENT DATA WERE FOUND
      do ns = 1,nlsegs
        do nr = 1,nR2L(ns)
          if (abs(facASLT(R2L(ns,nr),1)+9.0).lt.0.01) go to 993
        end do
      end do
 
      return

991   report(1) = 'error opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'error reading file near line'
      report(2) = fnam
      report(3) = statline
      go to 999

993   report(1) = 'did not find segment '//rsegs(R2L(ns,nr))
      report(2) = ' in file'
      report(3) = fnam
      go to 999

999   call stopreport(report)
      end

      function missing(value)
      implicit none
      logical missing
      real value
      missing = .false.
      if (abs(value+99.0).lt.0.01) missing = .true.
      end
