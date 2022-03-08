      SUBROUTINE lndDSN(dsnstr,tioscen,lentioscen,dsn)
      !{
         implicit none
         include '../../../lib/inc/standard.inc'
         include '../../../lib/inc/locations.inc'

         character*200 filename
c         integer       dfile
c         parameter     (dfile=10)
c         integer       err
c         character*200 line

         character*4   dsnstr
         integer       dsn

         logical       comment
         external      comment

         character*25  tioscen
         integer       lentioscen

         print*,'in lndDSN'
         print*,catdir
         print*,lentioscen
         print*,tioscen
         !call readcontrol_Lioscen(lscen,lenlscen,ioscen)
         !call lencl(ioscen,lenioscen)
         filename = catdir//'iovars/'//tioscen(:lentioscen)//'/perlnd'
         print*,filename
         open(dfile+2,file=filename,status='old',iostat=err)
         if (err.ne.0) go to 991

         do
         !{
            read(dfile+2,'(a100)',err=992,end=310) line
            print*,line
            call d2x(line,last)
            if (comment(line)) cycle
            if (line(:20).eq.'                    ') cycle
c            print*,line(26:29),'<>',C4_tStr
            if (line(26:29).eq.dsnstr) then
            !{
               read(line(:3),*) dsn
               go to 310
            !}
            end if
         !}
         end do
310      close (dfile+2)

         return

991      report(1) = 'Error 991b Problem opening file:'
         report(2) = fnam
         write(report(3),*)'iostat error = ',err
         go to 999

992      report(1) = 'Error 992c Problem reading file: near line:'
         report(2) = fnam
         report(3) = line
         go to 999

999   call stopreport(report)

      !}
      END
