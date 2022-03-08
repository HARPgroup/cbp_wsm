      parameter (nmax=14)
      integer idir(nmax),imon(nmax),jmon(nmax) !  ??, cell i, cell j
      integer nindx(nmax)
      real cros(nmax),xfact(nmax)
      real q(nmax)
      character*8 nwis(nmax)    ! station name
      character*4 year
c
c
c
      open(11,file='h:\potomac\ch3d\bmf_pk.inp',status='old')
      do iskip=1,33
        read(11,*)
      end do
      read(11,*) nst
      write(*,*) 'nst=',nst
      read(11,*)
      do i=1,nst
        read(11,'(3i8,6x,a8)') idir(i),imon(i),jmon(i),nwis(i)
        if(idir(i).le.2) then
          cros(i)=1.
        else
          cros(i)=-1.
        endif
      end do
      close(11)
      q(nst)=750.
c
c
c
      do i=1,nst
        do j=1,nst
          if(nwis(j).eq.nwis(i)) then
            xfact(i)=xfact(i)+1.
            nindx(i)=j
          endif
        end do
        lunit=nindx(i)+50
        open(lunit,file='discharge_'//nwis(i)//'.txt',
     $        status='old')
        do ihd=1,25
          read(lunit,*)
        end do
        write(*,*) i,xfact(i),nindx(i)
      end do
      stop
c
c
c
      do iyear=1994,1994
        write(year,'(i4)') iyear
        open(13,file='fort.13_'//year,status='unknown')
        leap=mod(iyear,4)
        if(leap.eq.0) then
          nday=366
        else
          nday=365
        endif
        do iday=-5,nday
          write(13,201) iday,0
          read(12,*) iyr,imn,idy,(q(j),j=1,nst-1)
          i=iyr-1996
          q(8)=q(8)+qmonth(i,imn)
          do j=1,nst
            write(13,202) imon(j),jmon(j),cros(j)*q(j)
          end do
        end do
        close(13)
        do iback=1,6
          backspace(12)
        end do
      end do
 201  FORMAT(2I8)   
 202  FORMAT(2I8,F8.0)
c
c
c
      stop
      end
