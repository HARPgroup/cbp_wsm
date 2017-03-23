      implicit none
      include 'Rstats.inc'

      character*200 filename
      integer       lenfilename
      integer       fileptr
      parameter (fileptr=12)
      integer year1, year2
      character*20  postfix
      integer lenpostfix
      integer i
      character*200 intro
      integer lenintro

      read*, rscen,year1,year2,postfix, intro
      call lencl(rscen, lenrscen)
c      call lencl(rseg,  lenrseg );
      call lencl(postfix, lenpostfix)

      filename = outwdmdir//'river/'//rscen(:lenrscen)//
     .            '/stream/'//rscen(:lenrscen)//'_'//
     .            postfix(:lenpostfix)//'_FLO'//'.stats.csv'
      call lencl(filename, lenfilename)
      print*,'>',filename(:lenfilename),'<'


      open(fileptr,file=filename(:lenfilename),
     .     status='NEW',
     .     iostat=err)
      print*,err
      if (err.ne.0) stop 'error opening FLO STAT file '

      call lencl(intro, lenintro)
      write(fileptr,'(A)') intro(:lenintro)
      write(fileptr,'(A29,$)') '     Scenario,      Segment, '
      do i=year1,year2
          write(fileptr, '(I13,A,$)'), i, ', '
      end do

      write(fileptr,*)
      close(fileptr)

1234  FORMAT(A)


      end
