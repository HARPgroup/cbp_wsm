      implicit none
      integer icell1,jcell1
      parameter ( icell1 =178,  jcell1 = 282 )
      real q(icell1,jcell1),qf(15)
      real t(icell1,jcell1),tf(15)
      real totalq(icell1,jcell1,1993:2000)  ! accumulation
      real totalt(icell1,jcell1,1993:2000)  ! accumulation
      character*4 year
      integer i,j,iyr,iday,idy,itm,icnt
c
c
c
	print*,1
      itm=0
      do iyr=1993,2000
	print*,2
        write(year,'(i4)') iyr
        open(33,file='hyd12/fort.33_new_'//year,status='old')
        open(32,file='wq710/fort.33_new_'//year,status='old')
        icnt=0
	print*,3
        do j = 1,jcell1  ! set to zero for year
          do i = 1,icell1
            totalq(i,j,iyr) = 0.0
            totalt(i,j,iyr) = 0.0
          end do
        end do
	print*,4
        do  ! loop over days in year
          icnt=icnt+1
          read(33,201,end=111) iday,itm
          read(32,201) idy,itm
          if(iday.ne.idy) write(*,*)'Error at day',iday,idy
          do j=1,jcell1
            read(33,211)(q(i,j),i=1,icell1)
            read(32,211)(t(i,j),i=1,icell1)
          end do                                            ! j loop

          do j = 1,jcell1   ! accumulate
            do i = 1,icell1
              totalq(i,j,iyr) = totalq(i,j,iyr) + q(i,j)
              totalt(i,j,iyr) = totalt(i,j,iyr) + t(i,j)
            end do
          end do
        end do                                              ! record loop
	print*,5
 111    close(33)
        close(32)
        write(*,*) iyr,icnt-1
        open(11,file='hyd12_wq710_'//year,status='unknown')
        do j = 1,jcell1
          do i = 1,icell1
            if (totalq(i,j,iyr).ge.0.1.or.totalt(i,j,iyr).ge.0.1) then
              write(11,*)i,',',j,',',totalq(i,j,iyr),',',totalt(i,j,iyr)
            end if
          end do
        end do
        close(11)
      end do                                                ! year loop
c
c
c
 201  format(2i8)   
 211  format(22f7.1)
 301  FORMAT(2I8)   
 302  FORMAT(2I8,F8.0)
 303  FORMAT(2I5,4f6.2)
 304  FORMAT(2I5)   
c
c
c
      stop
      end
      
