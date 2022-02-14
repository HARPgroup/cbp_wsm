************************************************************************
******** GET THE DATA AT THE BREAKPOINTS                              **
************************************************************************
      subroutine getPoundBreaksEOS(
     I                          rseg,numsegs,l2r,PBfile,nPB,
     I                          BMPconname,nBmpCon,
     O                          PoundsReduced)

      implicit none
      include 'mbtc.f'
      character*13 ctemp
      logical found
      integer i,k,l,nt,kk,ll
      integer nPB,lastPB,numsegs,nb,nb1,nb2
      integer Pnb(maxBMPcon)
      integer nl2r
      character*13 Trseg,Tl2r
      character*4 river,land
      character*3 Tlu,Tcon
      logical scompcase         ! string comparison routines
      external scompcase
      real Tpounds
C      character*1 cconst  ! temp var to read constrained


********* temp writing variables
c      logical writeout
c      data writeout /.true./ ! all input verified 6/27/2008

*********** END DECLARATIONS *******************************************
      river = 'rseg'
      land = 'lseg'

      do nt = 1, maxTimeBreaks               ! initialize BmpAcres
        do nb = 1,maxBMPcon
          do i = 1, maxL2R
            do l = 1,nlu
              PoundsReduced(nt,nb,i,l) = 0.0
            end do
          end do
        end do
      end do

      do nt = 1,nPB

        call trims(PBfile(nt),lastPB)              ! open bmp use file
        fnam= ScenDatDir//'river/bmps/bmp_loadredux_EOS_'//
     .             PBfile(nt)(:lastPB)//'.csv'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991
        read(dfile,'(a100)',err=996)line            ! read header line
        call d2x(line,i)

        call findcomma(line,i)    ! check that first column is river
        ctemp = line(:i-1)
        call trims(ctemp,i)
        if (.not.scompcase(ctemp(:i),river)) go to 992
        call shift(line)

        call findcomma(line,i)    ! check that second column is land
        ctemp = line(:i-1)
        call trims(ctemp,i)
        if (.not.scompcase(ctemp(:i),land)) go to 993
        call shift(line)

        call findcomma(line,i)    ! landuse
        call shift(line)

        do nb = 1,nBmpCon
          Pnb(nb) = -9
        end do
        do nb1 = 1,nBmpCon
          call findcomma(line,i)
          ctemp = line(:i-1)
          if (debug) print*,'ctemp-> ',ctemp
          call trims(ctemp,i)
          if ( ctemp(:2).eq.'  ' ) exit
          found = .false.
          do nb2 = 1,nBmpCon
            if (debug) print*,BMPconname(nb2)
            if (.not.scompcase(ctemp(:i),BMPconname(nb2))) cycle
            Pnb(nb1) = nb2
            found = .true.
          end do
          if ( found .eqv. .false. ) go to 998
          call shift(line)
        end do

        do ! loop over whole file
          read(dfile,'(a100)',err=997,end=111)line
          call d2x(line,last)

          read(line,*) Trseg
          if (Trseg.ne.rseg) cycle  ! search for rseg only

          if (line(100-3:100).ne.'    ') go to 990 ! check for long line

          call shift(line)          ! get land seg
          read(line,*) Tl2r
          found = .false.
          do i = 1,numsegs
            if (scompcase(Tl2r,l2r(i))) then
              found = .true.
              nl2r = i
              exit
            end if
          end do
          if (.not.found) go to 995

CBHATT          call shift(line)   ! get bmp name, do nothing with it
CBHATT          read(line,*) TBmpName

          call shift(line)        ! get land use
          read(line,*) Tlu
          found = .false.
          do l = 1,nlu
            if (scompcase(Tlu,luname(l))) then
              found = .true.
              ll = l
              exit
            end if
          end do
          if (.not.found) go to 994

c          call shift(line)     ! get BMP constituent
c          read(line,*) Tcon
c          found = .false.
c          do nb = 1,nBmpCon
c            if (scompcase(Tcon,BMPconname(nb))) then
c              found = .true.
c              Pnb = nb
c              exit
c            end if
c          end do
c          if (.not.found) go to 998

          do nb = 1,nBmpCon
            call shift(line)    ! read pounds
            call fread(line,Tpounds)
            if ( Pnb(nb) .eq. -9 ) cycle
            if (debug) print*,'Pnb-> ',nb,',',Pnb(nb)
            PoundsReduced(nt,Pnb(nb),nl2r,ll) = 
     .                  PoundsReduced(nt,Pnb(nb),nl2r,ll) + Tpounds
          end do

C          call shift(line)
C          read(line,*,err=987,end=988) cconst 
C          call lowercase(cconst)
C          if (cconst.eq.'y') then
C            constrained(nBmp,nt,nl2r,ll) = .true.
C          else
C            constrained(nBmp,nt,nl2r,ll) = .false.
C          end if
           
        end do
111     close (dfile)
 
      end do
      if (writeout) then
        fnam = outdir//'input/testPoundsBMPsOoutNew.csv'
        open(99,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

        write(99,*) 'BMP pounds'
C        write(99,332) 'time,lseg,nlu',
C     .                (',',BmpName(nBmp),nBmp=1,nBmpTypes)
        write(99,*) 'time,lseg,nlu',(',',BMPconname(nb),nb=1,nBmpCon)
        do nt = 1,nPB
          do i = 1,numsegs
            do l = 1,nlu
              write(99,333) PBfile(nt)(:lastPB),',',l2r(i),',',
     .                      luname(l),
     .          (',',PoundsReduced(nt,nb,i,l),nb=1,nBmpCon)
            end do
          end do
        end do
        close(99)
332     format(a13,100(a1,a20))
333     format(a13,a1,a6,a1,a3,200(a1,f8.3))
      end if
      return
*********** ERROR SPACE
C987   report(1) = 'problem reading constrained state in file'
C      report(2) = fnam
C      report(3) = 'should either be y or n'
C      go to 999
C
C988   report(1) = 'problem reading constrained state in file'
C      report(2) = fnam
C      report(3) = 'need an extra column'
C      go to 999
C
989   report(1) = 'found undefined BMP type '//TBmpName
      report(2) = ' in file '
      report(3) = fnam
      go to 999

990   report(1) = 'problem in ETM:  character variable not long '
      report(2) = 'enough to read entire file, modify:'
      report(3) =
     .     './code/src/etm/make_binary_transfer_coeffs/getpoundbreaks.f'
      go to 999

991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'first column must be the River segment and '
      report(2) = '   must have a title line in file'
      report(3) = fnam
      go to 999

993   report(1) = 'second column must be the land segment and '
      report(2) = '   must have a title line in file'
      report(3) = fnam
      go to 999

994   report(1) = 'could not make sense of land use '//Tlu
      report(2) = ' found in file'
      report(3) = fnam
      go to 999

995   report(1) = 'found unexpected land segment '//Tl2r
      report(2) = ' associated with '//Trseg//' in file '
      report(3) = fnam
      go to 999

996   report(1) = 'Could not read first line in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

997   report(1) = 'Could not read file: near line: '
      report(2) = fnam
      report(3) = line
      go to 999

998   report(1) = 'unallowable BMP constituent '//ctemp//' in file'
      report(2) = fnam
      report(3) = 'allowable types in iovars - bmp_constituents'
      go to 999

999   call stopreport(report)

      end


