      parameter ( icell1 =178,  jcell1 = 282 )
      real q(icell1,jcell1),qf(15)
      integer imon(15),jmon(15),ic(12)
      character*4 year
      data ic/3,4,5,7,2,1,6,9,10,11,8,12/
c
c
c
      itm=0
      do iyr=1993,1999
        write(year,'(i4)') iyr
        open(33,file='fort.33_new_'//year,status='unknown')
        open(13,file='fort.13_new_'//year,status='unknown')
        open(11,file='flows_year_'//year//'.prn',
     $        form='unformatted',status='old')
        open(12,file='modeled_gages_'//year//'.prn',
     $        form='unformatted',status='old')
        icnt=0
        read(12) ncells,imon,jmon
c        write(*,*) imon
c        write(*,*) jmon
        do
          icnt=icnt+1
          read(11,end=111) iday
          write(33,201) iday,itm
          read(12) idy
          write(13,301) idy,itm
          if(iday.ne.idy) write(*,*)'Error at day',iday,idy
          read(11)((q(i,j),i=1,icell1),j=1,jcell1)
          read(12) (qf(j),j=1,ncells)
          if(q(46,24).lt.0.) then
            qf(3)=qf(3)+q(46,24)
            if(qf(3).lt.0.) write(*,*) iday,3,qf(3),q(46,24)
            q(46,24)=0.
          endif
          if(q(54,40).lt.0.) then
            qf(5)=qf(5)+q(54,40)
            if(qf(5).lt.0.) write(*,*) iday,5,qf(5),q(54,40)
            q(54,40)=0.
          endif
          if(q(60,44).lt.0.) then
            qf(7)=qf(7)+q(60,44)
            if(qf(4).lt.0.) write(*,*) iday,7,qf(7),q(60,44)
            q(60,44)=0.
          endif
          do j=1,jcell1
            write(33,211)(q(i,j),i=1,icell1)
          end do                                            ! j loop
          do j=1,12
c            write(*,*) j,ic(j),imon(ic(j)),jmon(ic(j)),qf(ic(j))
            write(13,302) imon(ic(j)),jmon(ic(j)),qf(ic(j))
          end do
          write(13,302) 134,282,-750.
        end do                                              ! record loop
 111    close(11)
        write(*,*) iyr,icnt-1
      end do                                                ! year loop
c
c
c
 201  format(2i8)   
 211  format(22f7.1)
 301  FORMAT(2I8)   
 302  FORMAT(2I8,F8.0)
c
c
c
      stop
      end
      
