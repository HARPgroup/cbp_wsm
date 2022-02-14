      character*100 fnam
      read(*,'(a)'),fnam
      open(11,file=fnam,status='old',iostat=ierr)
      if (ierr.eq.2) stop

      n = 0
      do 
        read(11,*,end=111) iyear
        if (iyear.gt.1984.and.iyear.lt.2006) n = n + 1
      end do
111   close(11)

      do i = 1,100
        if (fnam(i:i).eq.' ') exit
      end do
      print*,fnam(i-19:i-7),',',fnam(i-4:i-1),',',n
      end 
