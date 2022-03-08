      subroutine read_s2r(s2rfnam,lrsegs,C_conx,nlrsegs,nconx,s2rfac)

      include 'rpa.inc'
      real F_flo,F_tnx,F_tpx,F_sed

      fnam = s2rfnam
      print*,fnam

      open (dfile,file=fnam,status='old',iostat=err)
      if ( err.ne.0 ) go to 991

      read (dfile,'(a300)',err=992) dline

      call d2x(dline,last)
      read (dline,*,err=993) xdline
      call lencl(xdline,lenxdline)
      print*,'>',xdline(:lenxdline),'<'
      if ( xdline(:lenxdline) .ne. 'riverseg'     ) go to  994

      call shift(dline)
      call d2x(dline,last)
      read (dline,*,err=993) xdline
      call lencl(xdline,lenxdline)
      print*,'>',xdline(:lenxdline),'<'
      if ( xdline(:lenxdline) .ne. 'landseg'  ) go to  994

      call shift(dline)
      call d2x(dline,last)
      read (dline,*,err=993) xdline
      call lencl(xdline,lenxdline)
      print*,'>',xdline(:lenxdline),'<'
      if ( xdline(:lenxdline) .ne. 'flo'//C3lu  ) go to  994

      call shift(dline)
      call d2x(dline,last)
      read (dline,*,err=993) xdline
      call lencl(xdline,lenxdline)
      print*,'>',xdline(:lenxdline),'<'
      if ( xdline(:lenxdline) .ne. 'tnx'//C3lu  ) go to  994

      call shift(dline)
      call d2x(dline,last)
      read (dline,*,err=993) xdline
      call lencl(xdline,lenxdline)
      print*,'>',xdline(:lenxdline),'<'
      if ( xdline(:lenxdline) .ne. 'tpx'//C3lu  ) go to  994

      call shift(dline)
      call d2x(dline,last)
      read (dline,*,err=993) xdline
      call lencl(xdline,lenxdline)
      print*,'>',xdline(:lenxdline),'<'
      if ( xdline(:lenxdline) .ne. 'sed'//C3lu  ) go to  994

      do
      !{
         read(dfile,*,err=995,end=211) rseg,lseg,F_flo,F_tnx,F_tpx,F_sed
         print*,"S2R READING ",rseg,lseg,F_flo,F_tnx,F_tpx,F_sed

         lrsegfound = .false.
         do ilrsegs = 1,nlrsegs
            if ( lseg//rseg .eq. lrsegs(ilrsegs) ) then
               lrsegfound = .true.
               exit
            end if
         end do
         if ( lrsegfound .eqv. .false. ) cycle
         print*,"S2R ASSIGN  ",lrsegs(ilrsegs),F_flo,F_tnx,F_tpx,F_sed
         do iconx = 1,nconx
            if (C_conx(iconx) .eq. 'nh3n' ) then
               s2rfac(ilrsegs,iconx) = F_tnx
            else if (C_conx(iconx) .eq. 'nh3n' ) then
               s2rfac(ilrsegs,iconx) = F_tnx
            else if (C_conx(iconx) .eq. 'no3n' ) then
               s2rfac(ilrsegs,iconx) = F_tnx
            else if (C_conx(iconx) .eq. 'orgn' ) then
               s2rfac(ilrsegs,iconx) = F_tnx
            else if (C_conx(iconx) .eq. 'po4p' ) then
               s2rfac(ilrsegs,iconx) = F_tpx
            else if (C_conx(iconx) .eq. 'orgp' ) then
               s2rfac(ilrsegs,iconx) = F_tpx
            else if (C_conx(iconx) .eq. 'sedm' ) then
               s2rfac(ilrsegs,iconx) = F_sed
            end if

         end do
      !}
      end do
211   close(dfile)

      return

991   report(1) = 'Problem: Unable to open file: '
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'Problem: reading file: '
      report(2) = fnam
      report(3) = dline
      go to 999

993   report(1) = 'Problem: parsing file line: '
      report(2) = fnam
      report(3) = xdline
      go to 999

994   write(report(1),*) 'Problem: parsing file header: '//fnam
      report(2) = dline
      report(3) = xdline(:lenxdline)
      go to 999

995   report(1) = 'LR Segment not found'
      report(2) = lseg//'_'//rseg
      report(3) = ''
      go to 999

999   call stopreport(report)


      end
