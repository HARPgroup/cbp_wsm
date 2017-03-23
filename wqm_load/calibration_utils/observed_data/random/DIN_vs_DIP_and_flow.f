      implicit none
      character*100 fnam
      character*13 rseg

      integer ierr

      integer fnh3,fno3,fpo4,ftn,ftp,fflow
      data fnh3,fno3,fpo4,ftn,ftp,fflow
     .    /11,12,13,14,15,16/
   
      read(*,'(a)'),rseg

      fnam = 'NH4X/'//rseg//'.ONH4X' 
      open(fnh3,file=fnam,status='old',iostat=ierr)
      if (ierr.eq.2) stop

      fnam = 'NO3X/'//rseg//'.ONO3X' 
      open(fno3,file=fnam,status='old',iostat=ierr)
      if (ierr.eq.2) stop

      fnam = 'PO4X/'//rseg//'.OPO4X' 
      open(fpo4,file=fnam,status='old',iostat=ierr)
      if (ierr.eq.2) stop

      fnam = 'TOTN/'//rseg//'.OTOTN' 
      open(ftn,file=fnam,status='old',iostat=ierr)
      if (ierr.eq.2) stop

      fnam = 'TOTP/'//rseg//'.OTOTP' 
      open(ftp,file=fnam,status='old',iostat=ierr)
      if (ierr.eq.2) stop

      fnam = 'FLOW/'//rseg//'.OFLOW' 
      open(fflow,file=fnam,status='old',iostat=ierr)
      if (ierr.eq.2) stop

      n = 0
      do 
        read(11,*,end=111) iyear
        if (iyear.gt.1999) n = n + 1
      end do
111   close(11)
      print*,fnam(6:18),' ',n
      end 
