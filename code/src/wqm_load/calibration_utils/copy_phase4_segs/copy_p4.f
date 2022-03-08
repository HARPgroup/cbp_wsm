      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      integer year,month,day,hour,zero,i
      real value
      character*3 p4seg,dummy
      character*13 basin,subbasin
      integer lenbasin,lensub,lenp4seg,nf

      character*4 suffix(13)
      data suffix /'chla','doxx','flow','nh3x','no3x','orgn','orgp',
     .             'po4x','tocx','totn','totp','tssx','wtmp'/

      read*,rseg,p4seg,basin,subbasin
      call lencl(p4seg,lenp4seg)
      call lencl(basin,lenbasin)
      call lencl(subbasin,lensub)

      do nf = 1,13
        fnam = '/p4/p4/outputs/'//basin(:lenbasin)//'/calib/'//
     .          subbasin(:lensub)//'/s'//p4seg(:lenp4seg)//
     .          '.'//suffix(nf)
        open(11,file=fnam,status='old',iostat=err)
        if (err.ne.0) then
          print*,' could not open file.  Error = ',err
          print*,fnam
          cycle
        end if

        call uppercase(suffix(nf))
        fnam = outdir//'river/daily/phase4/'//rseg//'.'//suffix(nf)
        open(12,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) then
          print*,' could not open file.  Error = ',err
          print*,fnam
          cycle
        end if

        do i = 1,26
          read(11,*) dummy
        end do

        do
          read(11,*,end=111) dummy,year,month,day,hour,zero,value
          write(12,*,err=951),
     .          year,',',month,',',day,',',hour,',',zero,',',value
        end do

111     close(11)
        close(12)

      end do
      return
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

999   call stopreport(report)
      end
