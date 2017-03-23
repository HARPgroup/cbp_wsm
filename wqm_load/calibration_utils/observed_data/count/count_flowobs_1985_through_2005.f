      character*100 fnam

      integer year,month,day,hour,minute
      real value

      read(*,'(a)'),fnam
      do i = 1,100
        if (fnam(i:i).eq.' ') exit
      end do
      do j = 1,2

        if (j.eq.2) fnam(i-10:i-7) = '0003'
        open(11,file=fnam,status='old',iostat=ierr)
        if (ierr.eq.2) cycle

        n = 0
        do 
          read(11,*,end=111) year,month,day,hour,minute,value
          if (year.ge.1985.and.year.le.2005) n = n + 1
        end do
111     close(11)

        print*,fnam(i-19:i-7),',',fnam(i-4:i-1),',',n

      end do
      end 
