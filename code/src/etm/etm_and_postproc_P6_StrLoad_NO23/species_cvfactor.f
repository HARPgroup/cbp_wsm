
      SUBROUTINE SPECIES_CVFACTOR(filename,tspecies1,tspecies2,factor)
      !{
         implicit none
         include '../../lib/inc/standard.inc'

         character*200 filename
         character*4   tspecies1,tspecies2
         real          factor

         logical       comment
         external      comment

         logical       found

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
            if (line(1:4).eq.tspecies1.and.line(10:13).eq.tspecies2)then
            !{
               read(line(19:),*) factor
               found = .true.
               goto 410
            !}
            end if
         !}
         end do
         if ( found .eqv. .false. ) goto 993

410      close (dfile)

         return

991      report(1) = 'Error 991c Problem opening file:'
         report(2) = fnam
         write(report(3),*)'iostat error = ',err
         go to 999

992      report(1) = 'Error 992c Problem reading file: near line:'
         report(2) = fnam
         report(3) = line
         go to 999

993      report(1) = 'Error 993c Problem factor not found '
         report(2) = tspecies1
         report(3) = tspecies2
         go to 999

999   call stopreport(report)

      !}
      END
