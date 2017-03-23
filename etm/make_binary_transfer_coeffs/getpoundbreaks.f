************************************************************************
******** GET THE DATA AT THE BREAKPOINTS                              **
************************************************************************
      subroutine getPoundBreaks(
     I                          rseg,numsegs,l2r,PBfile,nPB,
     I                          BMPconname,nbcon,
     O                          PoundsReduced)

      implicit none
      include 'mbtc.f'
      character*13 ctemp
      logical found
      integer i,k,l,nt,kk,ll
      integer nPB,lastPB,numsegs,nb
      integer Pnb
      integer nl2r
      character*13 Trseg,Tl2r
      character*4 river,land
      character*3 Tlu,Tcon
      logical scompcase         ! string comparison routines
      external scompcase
      real Tpounds
C      character*1 cconst  ! temp var to read constrained


********* temp writing variables
      logical writeout
      data writeout /.false./ ! all input verified 6/27/2008

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
        fnam= ScenDatDir//'river/bmppounds/bmppounds_'//
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

          call shift(line)   ! get bmp name, do nothing with it
          read(line,*) TBmpName

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

          call shift(line)     ! get BMP constituent
          read(line,*) Tcon
          found = .false.
          do nb = 1,nbcon
            if (scompcase(Tcon,BMPconname(nb))) then
              found = .true.
              Pnb = nb
              exit
            end if
          end do
          if (.not.found) go to 998
               
          call shift(line)    ! read pounds
          call fread(line,Tpounds)
          PoundsReduced(nt,Pnb,nl2r,ll) = 
     .                  PoundsReduced(nt,Pnb,nl2r,ll) + Tpounds

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
        fnam = tree//'tmp/TestPoundsBMPsOout.csv'
        open(99,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

        write(99,*) 'BMP pounds'
        write(99,332) 'time,lseg,nlu',
     .                (',',BmpName(nBmp),nBmp=1,nBmpTypes)
        do nt = 1,nPB
          do i = 1,numsegs
            do l = 1,nlu
              write(99,333) PBfile(nt)(:lastPB),',',l2r(i),',',
     .                      luname(l),
     .          (',',PoundsReduced(nt,nb,i,l),nb=1,nbcon)
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

998   report(1) = 'unallowable BMP constituent '//Tcon//' in file'
      report(2) = fnam
      report(3) = 'allowable types in iovars - bmp_constituents'
      go to 999

999   call stopreport(report)

      end


