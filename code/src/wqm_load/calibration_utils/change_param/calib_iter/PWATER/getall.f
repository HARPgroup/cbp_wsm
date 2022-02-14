************************************************************************
** subroutines to read all of the data, share common blocks with main **
************************************************************************

************************************************************************
** subroutine get river data                                          **
************************************************************************
      subroutine getriver(rscen,lenrscen,
     O           rsegs,uniqindex,uniqid,dsid,nrsegs,fnam,err)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include '../../../../lib/inc/rsegs.inc'
      logical comment
      external comment
      integer ic

      nrsegs = 0
C      print*,'getting river data'
      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/rivernames.csv'
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
      subroutine getland(rscen,lenrscen,
     O                   lsegs,nlsegs,fnam,err)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include '../../../../lib/inc/lsegs.inc'
      logical comment
      external comment
      integer ic

      nlsegs = 0
C      print*,'getting land data'
      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/landnames.csv'
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
      subroutine getorphans(calscen,
     O                      orphans,surrogates,norphans,fnam,err)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'calib.inc'
      logical comment
      external comment
      integer ic

      call lencl(calscen,lencalscen)
      norphans = 0
      fnam = controldir//'calib/PWATER/'//calscen(:lencalscen)//
     .       '/'//calscen(:lencalscen)//'_orphans.csv'
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
      subroutine getR2L(
     I                  calscen,lsegs,nlsegs,uniqindex,orphans,norphans,
     O                  weight,R2L,nR2L,fnam,err)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'calib.inc'
      logical comment
      external comment
      integer ic,nr,fourid
      real test

      logical found
****************************************************************************
      call lencl(calscen,lencalscen)

      do nl = 1,nlsegs
        nR2L(nl) = 0
      end do
      do nl = 1,nlsegs
        do nr = 1,maxR2L
          R2L(nl,nr) = 0
          weight(nl,nr) = 0.0
        end do
      end do
      
      fnam = controldir//'calib/PWATER/'//calscen(:lencalscen)//
     .       '/'//calscen(:lencalscen)//'_river_land.csv'
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
          read(Tseg(5:8),'(i4)',err=994) fourid
          nr = uniqindex(fourid)
          
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
        if (nR2L(nl).eq.0) then
          found = .false.
          do nr = 1,norphans
            if (orphans(nr).eq.lsegs(nl)) found = .true.
          end do
          if (.not.found) then
             print*,'orphan ',lsegs(nl)
             go to 996
          end if
        else
          test = 0.0
          do nr = 1,nR2L(nl)
            test = test + weight(nl,nr)
          end do
          if (abs(test-1.0).gt.0.0021) go to 996
        end if
      end do

      return

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
      print*,'sum of weights = ',test
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
     O               facLandEvap,facLZSN,facINFILT,facIRC,facAGWR,
     O               facINTFW,facAGWETP,facKVARY)
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'calib.inc'
      real rdum
      integer nr
      character*(*) version
      character*200 statline

************ Calibration Parameters
      real Tbias(maxrsegs),Wstat(maxrsegs),Sstat(maxrsegs)
      real Qstat(maxrsegs),Bstat(maxrsegs),QaveRI(maxrsegs)
      real BaveRI(maxrsegs),Pbias(maxrsegs),VPbias(maxrsegs)
      real WBaveRI(maxrsegs),SBaveRI(maxrsegs),lo10bias(maxrsegs)
      real lo05bias(maxrsegs)

************* functions to calculate factors
      real calfacLandEvap,calfacLZSN,calfacINFILT,calfacIRC
      real calfacAGWR,calfacINTFW,calfacAGWETP,calfacKVARY
      external calfacLandEvap,calfacLZSN,calfacINFILT,calfacIRC
      external calfacAGWR,calfacINTFW,calfacAGWETP,calfacKVARY

      character*4   tversion
      character*200 statlineN
      character*200 statlineO

      real TbiasN(maxrsegs),WstatN(maxrsegs),SstatN(maxrsegs)
      real QstatN(maxrsegs),BstatN(maxrsegs),QaveRIN(maxrsegs)
      real BaveRIN(maxrsegs),PbiasN(maxrsegs),VPbiasN(maxrsegs)
      real WBaveRIN(maxrsegs),SBaveRIN(maxrsegs),lo10biasN(maxrsegs)
      real lo05biasN(maxrsegs)

      real TbiasO(maxrsegs),WstatO(maxrsegs),SstatO(maxrsegs)
      real QstatO(maxrsegs),BstatO(maxrsegs),QaveRIO(maxrsegs)
      real BaveRIO(maxrsegs),PbiasO(maxrsegs),VPbiasO(maxrsegs)
      real WBaveRIO(maxrsegs),SBaveRIO(maxrsegs),lo10biasO(maxrsegs)
      real lo05biasO(maxrsegs)

      integer iVER
      integer I_temp

      iVER = 0
      tversion = version

      do nr = 1,maxrsegs    ! set to -9 to test that calib data exists
        facLandEvap(nr) = -9.0
      end do

      call lencl(rscen,lenrscen)
      fnam = outdir//'river/summary/'//rscen(:lenrscen)//
     .             '_sum_stats_'//tversion//'.csv'
      open(dfile+1,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      read  (tversion, '(I4)') I_temp
      if ( I_temp .gt. iVER ) then
         write (tversion, '(I4)' ) I_temp-1
         do I_temp = 1,4
            if (tversion(I_temp:I_temp).eq.' ') 
     .          tversion(I_temp:I_temp)='0'
         end do
         fnam = outdir//'river/summary/'//rscen(:lenrscen)//
     .             '_sum_stats_'//tversion//'.csv'
      else
         fnam = outdir//'river/summary/'//rscen(:lenrscen)//
     .             '_sum_stats.csv'
      end if
      print*,'dfile+2', fnam

      open(dfile+2,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      fnam = outdir//'river/summary/'//rscen(:lenrscen)//
     .             '_sum_stats.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      print*,'dfile ',fnam

      read(dfile,'(a200)',end=100,err=992) statlineN
      read(dfile+2,'(a200)',end=100,err=992) statlineO
      call ryt(statlineN,dfile+1)
      do
        read(dfile,'(a200)',end=100,err=992) statlineN
        read(dfile+2,'(a200)',end=100,err=992) statlineO
        call ryt(statlineN,dfile+1)
        read(statlineN(5:8),'(i4)',err=992,end=992) nr
        nr = uniqindex(nr)
        call shift(statlineN)
        call shift(statlineO)
        read(statlineN,*,err=992,end=992)
     .           Tbias(nr),Wstat(nr),Sstat(nr),Qstat(nr),Bstat(nr),
     .           rdum,rdum,rdum,
     .           QaveRI(nr),BaveRI(nr),Pbias(nr),VPbias(nr),
     .           rdum,rdum,lo10bias(nr),lo05bias(nr)
        read(statlineO,*,err=992,end=992)
     .           TbiasO(nr),WstatO(nr),SstatO(nr),QstatO(nr),BstatO(nr),
     .           rdum,rdum,rdum,
     .           QaveRIO(nr),BaveRIO(nr),PbiasO(nr),VPbiasO(nr),
     .           rdum,rdum,lo10biasO(nr),lo05biasO(nr)

        if ( Tbias(nr)*TbiasO(nr).lt.0 )
     .             Tbias(nr) = 0.5 * Tbias(nr)
        if ( (Wstat(nr)-1)*(WstatO(nr)-1).lt.0 )
     .             Wstat(nr) = 1 + 0.5 * (Wstat(nr) - 1)
        if ( (Sstat(nr)-1)*(SstatO(nr)-1).lt.0 )
     .             Sstat(nr) = 1 + 0.5 * (Sstat(nr) - 1)
        if ( (Qstat(nr)-1)*(QstatO(nr)-1).lt.0 )
     .             Qstat(nr) = 1 + 0.5 * (Qstat(nr) - 1)
        if ( (Bstat(nr)-1)*(BstatO(nr)-1).lt.0 )
     .             Bstat(nr) = 1 + 0.5 * (Bstat(nr) - 1)
        if ( (QaveRI(nr)-1)*(QaveRIO(nr)-1).lt.0 )
     .             QaveRI(nr) = 1 + 0.5 * (QaveRI(nr) - 1)
        if ( (BaveRI(nr)-1)*(BaveRIO(nr)-1).lt.0 )
     .             BaveRI(nr) = 1 + 0.5 * (BaveRI(nr) - 1)
        if ( Pbias(nr)*PbiasO(nr).lt.0 )
     .             Pbias(nr) = 0.5 * Pbias(nr)
        if ( VPbias(nr)*VPbiasO(nr).lt.0 )
     .             VPbias(nr) = 0.5 * VPbias(nr)
        if ( lo10bias(nr)*lo10biasO(nr).lt.0 )
     .             lo10bias(nr) = 0.5 * lo10bias(nr)
        if ( lo05bias(nr)*lo05biasO(nr).lt.0 )
     .             lo05bias(nr) = 0.5 * lo05bias(nr)

        facLandEvap(nr) = calfacLandEvap(
     I                Tbias(nr),Wstat(nr),Sstat(nr),Qstat(nr),Bstat(nr),
     I                QaveRI(nr),BaveRI(nr),Pbias(nr),VPbias(nr),
     I                WBaveRI(nr),SBaveRI(nr),lo10bias(nr),lo05bias(nr))
        facLZSN(nr)     = calfacLZSN(
     I                Tbias(nr),Wstat(nr),Sstat(nr),Qstat(nr),Bstat(nr),
     I                QaveRI(nr),BaveRI(nr),Pbias(nr),VPbias(nr),
     I                WBaveRI(nr),SBaveRI(nr),lo10bias(nr),lo05bias(nr))
        facINFILT(nr)   = calfacINFILT(
     I                Tbias(nr),Wstat(nr),Sstat(nr),Qstat(nr),Bstat(nr),
     I                QaveRI(nr),BaveRI(nr),Pbias(nr),VPbias(nr),
     I                WBaveRI(nr),SBaveRI(nr),lo10bias(nr),lo05bias(nr))

        facIRC(nr)      = calfacIRC(
     I                Tbias(nr),Wstat(nr),Sstat(nr),Qstat(nr),Bstat(nr),
     I                QaveRI(nr),BaveRI(nr),Pbias(nr),VPbias(nr),
     I                WBaveRI(nr),SBaveRI(nr),lo10bias(nr),lo05bias(nr))
        facAGWR(nr)     = calfacAGWR(
     I                Tbias(nr),Wstat(nr),Sstat(nr),Qstat(nr),Bstat(nr),
     I                QaveRI(nr),BaveRI(nr),Pbias(nr),VPbias(nr),
     I                WBaveRI(nr),SBaveRI(nr),lo10bias(nr),lo05bias(nr))
        facINTFW(nr)    = calfacINTFW(
     I                Tbias(nr),Wstat(nr),Sstat(nr),Qstat(nr),Bstat(nr),
     I                QaveRI(nr),BaveRI(nr),Pbias(nr),VPbias(nr),
     I                WBaveRI(nr),SBaveRI(nr),lo10bias(nr),lo05bias(nr))
        facAGWETP(nr)   = calfacAGWETP(
     I                Tbias(nr),Wstat(nr),Sstat(nr),Qstat(nr),Bstat(nr),
     I                QaveRI(nr),BaveRI(nr),Pbias(nr),VPbias(nr),
     I                WBaveRI(nr),SBaveRI(nr),lo10bias(nr),lo05bias(nr))
        facKVARY(nr)    = calfacKVARY(
     I                Tbias(nr),Wstat(nr),Sstat(nr),Qstat(nr),Bstat(nr),
     I                QaveRI(nr),BaveRI(nr),Pbias(nr),VPbias(nr),
     I                WBaveRI(nr),SBaveRI(nr),lo10bias(nr),lo05bias(nr))

      end do

100   close(dfile)
101   close(dfile+2)
      close(dfile+1)

************** CHECK THAT THE RIVER SEGMENT DATA WERE FOUND
      do ns = 1,nlsegs
        do nr = 1,nR2L(ns)
          if (abs(facLandEvap(R2L(ns,nr))+9.0).lt.0.01) go to 993
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


      

      

************************************************************************
** subroutine get surface runoff from the land                        **
************************************************************************
      subroutine getSURO(clu,thislseg,parmscen,
     O                   avsuro)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'

      character*140 outline
      character*3 clu
      character*6 thislseg
      character*25 parmscen
      integer lenparmscen    

      integer startyear,nyears,ny
  
      real annsuro(nyearmax)
      real sumsuro, avsuro

*************** END DECLARATIONS ***************************************

      call lencl(parmscen,lenparmscen)

**********            OPEN FILE
      fnam = outdir//'hspf/land/out/'//
     .       clu//'/'//parmscen(:lenparmscen)//'/'//
     .             clu//thislseg(:6)//'.out'

      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

**********     loop over lines look for meaningful lines
       nyears = 0
       do
        read(dfile,'(a140)',err=992,end=111) outline
        if (err.ne.0) go to 992

        if (outline(2:9).eq.'PERVIOUS') then
          nyears = nyears + 1
          if (nyears.eq.1) read(outline(125:128),'(i4)')startyear
        end if

        if (outline(6:29).eq.'EXTNL INFLOWS & OUTFLOWS') then     ! if find this line in PWATER section
          read(dfile,'(a140)',err=992,end=111) outline
  
          if (outline(48:51).eq.'SURO') then
           read(dfile,'(a140)',err=992,end=111) outline
           read(dfile,'(a140)',err=992,end=111) outline
           read(outline(42:51),*) annsuro(nyears)
          end if
        end if

       end do

111   close(dfile)

********** get average annual SURO
      sumsuro = 0.

      do ny = 2, nyears 
       sumsuro = sumsuro + annsuro(ny)
      end do

      avsuro = sumsuro/real(nyears-1)
      
      return

********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'problem reading file - getall 992'
      report(2) = fnam
      report(3) = 'near line:'//outline
      go to 999

999   call stopreport(report)

      end


