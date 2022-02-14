************************************************************************
******** get the index of the HGMR for an lrseg                       **
************************************************************************
      subroutine gethgmr(
     I                   BmpTypeScen,
     I                   rseg,numsegs,l2r,
     I                   allHGMRs,nHGMR,
     O                   indHGMR)
      implicit none
      include 'mbtc.f'

      logical foundseg,foundhgmr
      integer i
      integer numsegs

      character(13) Trseg,Tl2r

**************END DECLARATION ******************************************
      do i = 1,numsegs  ! for later check
        indHGMR(i) = -999
      end do

******** open file
      call lencl(BmpTypeScen,lenrscen)
      fnam = ScenDatDir//'river/bmptypes/'//BmpTypeScen(:lenrscen)//'/'
     .       //'lrseg_HGMRs.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do     ! read the whole file
        read(dfile,'(a100)',err=992,end=111) line  
        call d2x(line,last)

        read(line,*,err=993,end=993) Trseg
        if (Trseg.ne.rseg) cycle

        call shift(line)  ! correct rseg if got to here

        read(line,*,err=994,end=994) Tl2r  ! read lseg

        foundseg = .false.  ! determine l2r index and store indHGMR
        do i = 1,numsegs
          if (Tl2r.eq.l2r(i)) then
            foundseg = .true.
            call shift(line)   ! read HGMR
            read(line,*,err=995,end=995) THGMR
            call lowercase(THGMR)

            foundhgmr = .false.  ! determine hgmr index
            do nHG = 1,nHGMR
              if (THGMR.eq.allHGMRs(nHG)) then
                foundhgmr = .true.
                indHGMR(i) = nHG
                exit
              end if
            end do
            if (.not.foundhgmr) go to 996

          end if
        end do
        if (.not.foundseg) go to 997

      end do
111   close(dfile)

      do i = 1,numsegs
        if (indHGMR(i).le.0) go to 998
      end do
            
      return

*********** ERROR SPACE ******************************************
991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'problem reading file: line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'Problem reading river segment in file:'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'Problem reading land segment in file:'
      report(2) = fnam
      report(3) = Trseg//' '//line
      go to 999

995   report(1) = 'Problem reading HGMR in file:'
      report(2) = fnam
      report(3) = Trseg//' '//Tl2r//' '//line
      go to 999

996   report(1) = 'found unexpected HGMR '//THGMR
      report(2) = ' associated with '//Trseg//' '//Tl2r//' in file '
      report(3) = fnam
      go to 999

997   report(1) = 'gethgmr found unexpected land segment '//Tl2r
      report(2) = ' associated with '//Trseg//' in file '
      report(3) = fnam
      go to 999

998   report(1) = 'Could not find all land segs for river seg, file'
      report(2) = fnam
      report(3) = 'river seg '//Rseg//' land seg '//l2r(i)
      go to 999

999   call stopreport(report)

      end


