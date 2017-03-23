      character*100 fnam
      read(*,'(a)'),fnam
      open(11,file=fnam,status='old',iostat=ierr)
      if (ierr.eq.2) stop

      n = 0
      do 
        read(11,*,end=111) iyear
        if (iyear.gt.1984.and.iyear.lt.2003) n = n + 1
      end do
111   close(11)
      print*,fnam(:58),' ',n
      end 
