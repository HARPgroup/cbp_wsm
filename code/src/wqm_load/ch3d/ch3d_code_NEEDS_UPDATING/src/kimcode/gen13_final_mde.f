      integer monday(12)
      integer idir(9),imon(9),jmon(9)
      real cros(9)
      real q(371,9)
      integer i1skip(3),i2skip(3),i3skip(3)
      character(30) head1
      character(10) head2
      character(11) head3
      byte ihd(10)
      data idir/ 4, 2, 1, 2, 3, 2, 1, 2, 3/
      data imon/62,14,37,40,43,37,33,36,76/
      data jmon/83,17,95, 1,10,16,19,23,63/
      data cros/-1, 1, 1, 1,-1, 1, 1, 1,-1/
      data i1skip/382,747,1112/
      data i2skip/1823,2188,2553/
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
      open(11,file='.\usgs_gage_files\01578310',status='old')
      open(10,file='.\cbp_loading\s770.afl',status='old')
      open(20,file='.\cbp_loading\s760.afl',status='old')
      open(21,file='.\chester_flow\s100_mde.out',status='old')
      open(31,file='.\chester_flow\s220_mde.out',status='old')
      open(32,file='.\chester_flow\s230_mde.out',status='old')
      open(41,file='.\chester_flow\s330_mde.out',status='old')
      open(51,file='.\chester_flow\s350_mde.out',status='old')
      open(61,file='.\chester_flow\s370_mde.out',status='old')
      open(62,file='.\chester_flow\s410_mde.out',status='old')
c
c
c
      do i=1,i1skip(iyr)
        read(11,*)
      end do
      do i=1,i3skip(iyr)
        read(10,*)
        read(20,*)
        read(21,*)
        read(31,*)
        read(32,*)
        read(41,*)
        read(51,*)
        read(61,*)
        read(62,*)
      end do
      do i=1,371
        read(11,*,end=111) head1,head1,head1,q(i,1)
        read(10,*,end=112) head2,q(i,2)
        read(20,*,end=112) head2,q(i,3)
 112    continue
        read(21,*,end=111) head2,q(i,4)
        read(31,*,end=111) head2,q(i,5)
        read(32,*,end=111) head2,qdum
        q(i,5)=q(i,5)+qdum
        read(41,*,end=111) head2,q(i,6)
        read(51,*,end=111) head2,q(i,7)
        read(61,*,end=111) head2,q(i,8)
        read(62,*,end=111) head2,qdum
        q(i,8)=q(i,8)+qdum
      end do
 111  rewind(11)
      rewind(21)
      rewind(31)
      rewind(32)
      rewind(41)
      rewind(51)
      rewind(61)
      rewind(62)
      do i=1,371
        iday=i-6
        write(13,201) iday,0
        do j=1,9
          write(13,202) imon(j),jmon(j),cros(j)*q(i,j)
        end do
      end do
 201  FORMAT(2I8)   
 202  FORMAT(2I8,F8.0)
c
c
c
      stop
      end
