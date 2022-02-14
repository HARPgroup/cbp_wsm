************************************************************************
** subroutine get EOF targets provided by NRI                         **
************************************************************************
      subroutine gettargets(
     I                      lsegs,nlsegs,tarscen,clu,
     O                      targets)
      implicit none
      include 'calib_sed.inc'

      character*800 dline
      character*13 ltemp
      character*(*) tarscen
      character*(*) clu
      integer lentarscen

      integer order
      logical found
      integer i,ns,ic  ! indices

      character*13 Tlseg
      real values(nlu)
      
      logical comment
      external comment
      logical foundlseg(maxlsegs)

**************END DECLARATION ******************************************

      do ns = 1,nlsegs
        foundlseg(ns) = .false.
      end do
          
************ open EOF targets file
      call lencl(tarscen,lentarscen)
      fnam = calibdir//'target/'//tarscen(:lentarscen)//
     .         '/sed_target.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a800)',err=996,end=992)dline ! read header line
      call d2x(dline,last)

      order = 0
      do ! find the order of the land use
        call shift(dline)
        order = order + 1
        call findcomma(dline,ic)
        ltemp = dline(:ic-1)
        call trims(ltemp,last)
        if (ltemp(:last).eq.' ') go to 993
        if (ltemp(:last).eq.clu) exit
      end do

      do   ! get all lsegs
        read(dfile,'(a800)',err=996,end=111) dline 
        call d2x(dline,last)
        if (comment(dline)) cycle
        if (dline(:3).eq.'end') exit

        read(dline,*,err=996,end=992) Tlseg,(values(i),i=1,order)
        found = .false.
        do ns = 1, nlsegs
          if (Tlseg .eq. lsegs(ns)) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) cycle   ! do not need this seg

        foundlseg(ns) = .true.
        targets(ns) = values(order)
      end do                  ! finish reading all land segments

111   close (dfile)

      do ns = 1,nlsegs
        if (.not.foundlseg(ns)) go to 997
      end do

      return

**************** ERROR SPACE *******************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'problem reading file: line: not enough entries'
      report(2) = fnam
      report(3) = dline
      go to 999

993   report(1) = ' land use '//clu//' was not found in file header'
      report(2) = fnam
      report(3) = dline
      go to 999

996   report(1) = 'error reading or parsing file: near line:'
      report(2) = fnam
      report(3) = dline
      go to 999

997   report(1) = 'did not find segment '//lsegs(ns)
      report(2) = fnam
      report(3) = ''
      go to 999

999   call stopreport(report)
      end 

