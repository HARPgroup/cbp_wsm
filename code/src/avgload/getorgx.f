      subroutine getorgx(
     I       filename,c3lu,
     O       fLORN,fLORP,fPIPX)

      implicit none
      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'

      character*200 filename
      character*3   c3lu
      real          fLORN,fLORP,fPIPX

      logical       comment
      external      comment

      logical found

      found = .false.


         open(dfile,file=filename,status='old',iostat=err)
         if (err.ne.0) go to 991

         do
         !{
            read(dfile,'(a100)',err=992,end=410) line
            call d2x(line,last)
            if (comment(line)) cycle
            if (line(:20).eq.'                    ') cycle
c            print*,line(26:29),'<>',C4_tStr
            if (line(1:3).eq.c3lu)then
            !{
               read(line( 5:9 ),*) fLORN
               read(line(11:15),*) fLORP
               read(line(17:21),*) fPIPX
               found = .true.
               goto 410
            !}
            end if
         !}
         end do
C         if ( found .eqv. .false. ) goto 993

410      close (dfile)
         if ( found .eqv. .false. ) goto 993

         return

991      report(1) = 'Error 991d Problem opening file:'
         report(2) = filename
         write(report(3),*)'iostat error = ',err
         go to 999

992      report(1) = 'Error 992d Problem reading file: near line:'
         report(2) = filename
         report(3) = line
         go to 999

993      report(1) = 'Error 993d Problem lu not found '
         report(2) = filename
         report(3) = c3lu
         go to 999

999   call stopreport(report)

      !}
      END
