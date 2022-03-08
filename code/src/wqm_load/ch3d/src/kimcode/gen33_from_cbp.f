      PARAMETER ( ICELL1 =178,  JCELL1 = 282 )
      real q(366,icell1,jcell1)
      integer im(5000),jm(5000)
c
c  iskip and jskip are for the fresh water inflow locations
c
      integer iskip(13),jskip(13),lyear(4),mday(12)
      data iskip/46,52,54,60,35,  2, 58,104,104,104,99,178,134/
      data jskip/24,21,40,44,72,106,138,252,253,254,216,169,282/
c
c
c
      character*2 year(4)
      data year/'93','94','95','96'/
      data lyear/365,365,365,366/
      data mday/0,31,59,90,120,151,181,212,243,273,304,334/
c
c
c
      open(21,file='pot2bay.csv',status='old')
      read(21,*)
      do
        read(21,*,end=222) ic,icb,il
        if(il.ne.0) then
          im(il)=ic/1000
          jm(il)=mod(ic,1000)
        endif
      end do
 222  close(21)      
c
c
c
      do iyr=1,2
        if(iyr.eq.4) then
          do j=3,12
            mday(j)=mday(j)+1
          end do
        endif
        open(33,file='fort.33_'//year(iyr),status='unknown')
        open(11,file='wsm_opt.'//
     $        year(iyr),status='old')
        do
          read(11,*,end=111) ic,iy,imn,id,dox,tmp,chl,dsq
c          write(*,*) ic,iy,imn
          i=mday(imn)+id
          q(i,im(ic),jm(ic))=dsq*31.3145
        end do
 111    close(11)        
c
c  make fall line flow zero
c
        do i=1,366
          do k=1,13
            q(i,iskip(k),jskip(k))=0.
          end do
        end do
c       
c
c

        do idy=1,lyear(iyr)
          write(33,201) idy-1,0
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
        close(33)
      end do
 201  FORMAT(2I8)   
 211  FORMAT(22F6.1)
c
c
c
      stop
      end
      
