************************************************************************
** sub to get land evaporation potential multiplier for ext sources   **
************************************************************************
      subroutine getevap(lseg,paramscen,evap)
      implicit none

      include '../inc/standard.inc'
      include '../inc/locations.inc'

      integer i,lwc
      integer lenps

      real evap

      logical scompare

      evap = -1.0

      call findopen(lwc)
      call lencl(paramscen,lenps)
      fnam = pardir//'common/'//paramscen(:lenps)//
     .       '/land_evap.csv'
      open (lwc,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(lwc,'(a1)') c
      Tseg = '0'
      do while (.not.scompare(Tseg,lseg))
        read(lwc,'(a10)',end=1001,err=1001) Tseg
        call d2x(Tseg,last)
        call findcomma(Tseg,last)
        Tseg = Tseg(:last-1)
      end do
      backspace lwc
      read(lwc,'(a100)',err=1002) line
      call d2x(line,last)
      call shift(line)
      call fread(line,evap)
      close (lwc)

      if (evap.lt.0.0) go to 1003



      return

*********** ERROR SPACE
991   report(1) = 'Could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999 

1001  report(1) = 'Could not find segment '//lseg//' in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

1002  report(1) = 'Could not read line for segment '//lseg
      report(2) = ' in file: '
      report(3) = fnam
      go to 999

1003  report(1) = 'invalid numbers for evaporation, segment'
     .            //lseg
      report(2) = 'evap   = XXXXXXXXXX'
      write(report(2)(10:19),'(f10.4)') evap
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end

