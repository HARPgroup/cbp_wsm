      PARAMETER ( ICELL1 = 76,  JCELL1 = 98,   IJMAX1  = 99 )
      integer idir(9),imon(9),jmon(9)
      real cros(9)
      real q(371,icell1,jcell1)
      integer ij(52,12)
      real wt(52,12),area(52)
      integer iskip(3),i3skip(3)
      character*3 seg(52)
      character*50 fname
      character(10) head2
      character(11) head3
      data idir/ 4, 2, 1, 2, 3, 2, 1, 2, 3/
      data imon/62,14,37,40,43,37,33,36,76/
      data jmon/83,17,95, 1,10,16,19,23,63/
      data cros/-1, 1, 1, 1,-1, 1, 1, 1,-1/
      data iskip/1823,2188,2553/
      data i3skip/4745,5110,5475/
c
c
c
      write(*,*)' year to extract (e.g. 97)'
      read(*,*) m_year
      iyr=m_year-96
c
c
c
      open(11,file='mde_hspf-ch3d_ij.csv',status='old')
      open(12,file='mde_hspf-ch3d_wt.csv',status='old')
      do ihd=1,2
        read(11,*)
        read(12,*)
      end do
      do i=1,34
        read(11,*) seg(i),(ij(i,j),j=1,11),area(i)
        read(12,*) seg(i),(wt(i,j),j=1,11)
        fname='.\chester_flow\s'//seg(i)//'_mde.out'
        lunit=40+i
        open(lunit,file=fname,status='old')
      end do
      close(11)
      close(12)
c
c
c
      open(11,file='cbp_hspf-ch3d_ij.csv',status='old')
      open(12,file='cbp_hspf-ch3d_wt.csv',status='old')
      do ihd=1,2
        read(11,*)
        read(12,*)
      end do
      do i=35,52
        read(11,*) seg(i),(ij(i,j),j=1,12),area(i)
        read(12,*) seg(i),(wt(i,j),j=1,12)
        if(i.le.37) then
          fname='.\cbp_loading\s'//seg(i)//'.AFL'
        else
          fname='.\cbp_loading\s'//seg(i)//'.out'
        endif
        lunit=40+i
        open(lunit,file=fname,status='old')
      end do
c
c
c
      do ifle=8,34
        lunit=40+ifle
        if(ifle.eq.11) goto 112
        do i=1,i3skip(iyr)
          read(lunit,*)
        end do
        do i=1,371
          read(lunit,*,end=111) head2,qval
          do j=1,11
            if(wt(ifle,j).ne.0.) then
              imon=ij(ifle,j)/1000
              jmon=mod(ij(ifle,j),1000)
              q(i,imon,jmon)=qval*wt(ifle,j)
            endif
          end do
        end do
 111    close(lunit)
 112    continue
      end do
c
c
c
      do ifle=38,52
        lunit=40+ifle
        do i=1,i3skip(iyr)
          read(lunit,*)
        end do
        do i=1,371
c          read(lunit,*,end=113) idum,head3,qval
          read(lunit,*,end=113) head2,qval
          do j=1,12
            if(wt(ifle,j).ne.0.) then
              imon=ij(ifle,j)/1000
              jmon=mod(ij(ifle,j),1000)
              q(i,imon,jmon)=qval*wt(ifle,j)
            endif
          end do
        end do
 113    close(lunit)
      end do
c
c
c
      do idy=1,371
        iday=idy-6
        write(33,201) iday,0
        DO J=1,JCELL1
          do i=1,icell1
            if(q(idy,i,j).gt.9999.) then
              write(*,*) idy,i,j,q(idy,i,j)
              q(idy,i,j)=9999.9
            endif
          end do
          write(33,211)(Q(idy,I,J),I=1,ICELL1)
        end do
      end do
 201  FORMAT(2I8)   
 211  FORMAT(22F6.1)
c
c
c
      stop
      end
