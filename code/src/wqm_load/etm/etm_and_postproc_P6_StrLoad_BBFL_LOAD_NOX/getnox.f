***** Program returns a value of NOX for a give value of TN
      subroutine getnox(
     I            rscen,rseg,AVG_TOTN,
     O            nox)

      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      real MINNOX, MAXNOX
      parameter ( MINNOX = 0.10 )
      parameter ( MAXNOX = 0.95 )
      real AVG_TOTN
      real tn, tn1, tn2
      real nox, nox1, nox2
      real area

      character*13 trseg
      character*6  tlseg
      integer lentrseg,lentlseg
      real         tacre

      tn = AVG_TOTN

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)


      call readcontrol_Rgeoscen(
     I                         rscen,lenrscen,
     O                         geoscen)
      call lencl(geoscen,lengeoscen)
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//
     .        '/land_water_area.csv'
      open(dfile+1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 901

      area = 0.0
      read(dfile+1,*)line ! header
      do
         read(dfile+1,*,err=902,end=101)trseg,tlseg,tacre
         call lencl(trseg,lentrseg)
         call lencl(tlseg,lentlseg)
         if ( rseg(:lenrseg) .eq. trseg(:lentrseg) ) then
            print*,trseg(:lentrseg),',',tlseg(:lentlseg),tacre
            area = area + tacre
         end if
      end do

101   close(dfile+1)

      print*,rseg(:lenrseg),', ',tn,area
      tn = tn / area
      print*,rseg(:lenrseg),' LbPA ',tn



      print*,ioscen(:lenioscen)
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/eor_nox'
      print*,fnam
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 901
      do
        tn1  = tn2
        nox1 = nox2
        read(dfile,*,err=902,end=102)tn2,nox2
        print*,'(',tn1,',',nox1,')','(',tn2,',',nox2,')'
        if(tn2 .gt. tn) exit
      end do
      nox = nox1 + (tn-tn1)*((nox2-nox1)/(tn2-tn1))
      print*,nox

      nox = nox / tn
c      print*,'reg NOX fraction = ',nox
      nox = max(MINNOX,nox)
      nox = min(MAXNOX,nox)

      return

102   close(dfile)

      go to 903

901   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

902   report(1) = 'error reading file'
      report(2) = fnam
      report(3) = ' '
      go to 999

903   report(1) = 'tn value outside the range of the table'
      write(report(2),*) 'tn= ',tn
      report(3) = 'expand the table'
      go to 999

999   call stopreport(report)

      end
