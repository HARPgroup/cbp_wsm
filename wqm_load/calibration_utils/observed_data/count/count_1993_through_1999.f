      character*100 fnam
      read(*,'(a)'),fnam
      open(11,file=fnam,status='old',iostat=ierr)
      if (ierr.eq.2) stop

      n = 0
      do 
        read(11,*,end=111) iyear
        if (iyear.gt.1992.and.iyear.lt.2000) n = n + 1
      end do
111   close(11)
      print*,fnam(6:18),' ',n
      end 
