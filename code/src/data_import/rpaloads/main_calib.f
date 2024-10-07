
      include 'rpa.inc'


      read*,sdate(1),edate(1),rpafnam,rpascen,s2rfnam

      call lencl(rpa,lenrpa)

      fnam = tree//'config/catalog/iovars/'//'p600'//
     .          '/rpa_to_wdm'
c      fnam = 'rpa_to_wdm'
      open (dfile+1,file=fnam,status='old',iostat=err)
      if ( err.ne.0 ) go to 891

      dline = 'START'
      ndsns = 0
      nconx = 0
c      read(dfile+1,'(a100)',err=892,end=111) dline
      do while ( dline(:3).ne.'END' )
      !{

         read(dfile+1,'(a100)',err=892,end=111) dline
         if ( comment(dline) ) cycle
         if ( dline(:3).eq.'END' ) cycle

         ndsns = ndsns + 1
c         read(dline,'( 0x,A4)',iostat=err) outcon(ndsns)
         read(dline,'(A4)',iostat=err) outcon(ndsns)
         read(dline,'( 8x,I4)',iostat=err) outdsn(ndsns)
         read(dline,'(16x,I4)',iostat=err)  ncons(ndsns)

         do icon = 1,ncons(ndsns)
            read(dline(24+(icon-1)*16+1:28+(icon-1)*16),'(A4)')
     .           inpcon(ndsns,icon)
c            read(dline(32+(icon-1)*16+1:36+(icon-1)*16),'(E4)')
            read(dline(32+(icon-1)*16+1:36+(icon-1)*16),*)
     .           confac(ndsns,icon)
            conxfound = .false.
            do iconx = 1,nconx
               print*,'>',C_conx(iconx),'<>',inpcon(ndsns,icon),'<'
              if ( C_conx(iconx) .eq. inpcon(ndsns,icon) ) 
     .             conxfound = .true.
            end do
            if ( conxfound .eqv. .false. ) then
               nconx = nconx + 1
               C_conx(nconx) = inpcon(ndsns,icon)
               I_conx(nconx) = nconx
            end if
         end do
         print*, outcon(ndsns), ' -> ',
     .           (inpcon(ndsns,icon), ' x ', 
     .            confac(ndsns,icon), ' + ',icon=1,ncons(ndsns))

c         read(dfile+1,'(a100)',err=892,end=111) dline

      !}
      end do

111   close (dfile+1)


      print*,(C_conx(icon),',',icon=1,nconx)
      print*,(I_conx(icon),',',icon=1,nconx)

C      return








      do ilrsegs = 1,maxlrsegs
         do iconx = 1,maxcons
            do iyear = lyear,hyear
               do imon = 1,12
                 lrsegloadinp(ilrsegs,iconx,iyear,imon) = -9.99D0
               end do
            end do
         end do
      end do
      do ilrsegs = 1,maxlrsegs
         do idsn = 1,maxdsns
            do iyear = lyear,hyear
               do imon = 1,12
                 lrsegloadout(ilrsegs,idsn,iyear,imon) =  0.0D0
               end do
            end do
         end do
      end do





      call lencl(rpafnam,lenrpafnam)
      fnam = tree//'input/unformatted/rpaload/'//rpa(:lenrpa)//'_'
     .           //rpafnam(:lenrpafnam)//'.csv'

      open (dfile,file=fnam,status='old',iostat=err)
      if ( err.ne.0 ) go to 991

      read (dfile,'(a300)',err=992) dline
C      xdline = 'START'
      read(dline,*,err=993) nyears
      call shift(dline)
      do iyear = 1,nyears
C         call d2x(dline,last)
         read (dline,*,err=993) years(iyear)
         call shift(dline)
         print*,years(iyear)
      end do

C      return

      read (dfile,'(a300)',err=992) dline

      call d2x(dline,last)
      read (dline,*,err=993) xdline
      call lencl(xdline,lenxdline)
      print*,'>',xdline(:lenxdline),'<'
      if ( xdline(:lenxdline) .ne. 'year'     ) go to  994

      call shift(dline)
      call d2x(dline,last)
      read (dline,*,err=993) xdline
      call lencl(xdline,lenxdline)
      print*,'>',xdline(:lenxdline),'<'
      if ( xdline(:lenxdline) .ne. 'riverseg' ) go to  994

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
      if ( xdline(:lenxdline) .ne. 'landuse'  ) go to  994

      call shift(dline)
      call d2x(dline,last)
      read (dline,*,err=993) xdline
      call lencl(xdline,lenxdline)
      print*,'>',xdline(:lenxdline),'<'
      if ( xdline(:lenxdline) .ne. 'nutrient' ) go to  994
      

      !! >> READ DATA FILE
      nlrsegs = 0
C      nyears  = 0
      do
      !{

         read(dfile,*,err=995,end=211) year,rseg,lseg,lux,nutx,
     .         (tload(i),i=1,12)
         print*,year,rseg,lseg,nutx,tload(1),tload(2)

         ! TODO if lux != rpa catch error

         lrsegfound = .false.
         do ilrsegs = 1,nlrsegs
            if ( lseg//rseg .eq. lrsegs(ilrsegs) ) then
               lrsegfound = .true.
               exit
            end if
         end do
         if ( lrsegfound .eqv. .false. ) then
         !{
            nlrsegs = nlrsegs + 1
            ilrsegs = nlrsegs
            lrsegs(nlrsegs) = lseg//rseg
         !}
         end if

         nutxfound = .false.
         do iconx = 1,nconx
            if ( nutx .eq. C_conx(iconx) ) then
               nutxfound = .true.
               exit
            end if
         end do

         if ( nutxfound .eqv. .false. ) go to 996

         do imon = 1,12
            lrsegloadinp(ilrsegs,iconx,year,imon) = tload(imon)
         end do

C         yearfound = .false.
C         do iyear=1,nyears
C            if (year .eq. years(iyear) ) then
C               yearfound = .true.
C               exit
C            end if
C         end do
C         if ( yearfound .eqv. .false. ) then
C            nyears = nyears + 1
C            years(nyears) = year
C         end if

      !}
      end do

211   close(dfile)




      !! >> SET MISSING LOADS AS ZERO FOR INPUT YEARS
      print*,'nyear = ',nyears
      do iyear=1,nyears
         do ilrsegs = 1,nlrsegs
            do iconx = 1,nconx
               do imon = 1,12
                  if(lrsegloadinp(ilrsegs,iconx,years(iyear),imon).lt.0)
     .             lrsegloadinp(ilrsegs,iconx,years(iyear),imon) = 0.0D0
               end do
            end do
         end do
      end do


      !! >> READ S2R FACTORS
      do ilrsegs = 1,nlrsegs
         do iconx = 1,nconx
            s2rfac(ilrsegs,iconx) = 1.0D0
         end do
      end do
      call read_s2r(s2rfnam,lrsegs,C_conx,nlrsegs,nconx,s2rfac)


      !! >> APPLY S2R FACTORS
      do iyear=1,nyears
         do ilrsegs = 1,nlrsegs
            do iconx = 1,nconx
               do imon = 1,12
                  print*,lrsegs(ilrsegs),C_conx(iconx),
     .               s2rfac(ilrsegs,iconx)
                  lrsegloadinp(ilrsegs,iconx,years(iyear),imon) = 
     .               lrsegloadinp(ilrsegs,iconx,years(iyear),imon) * 
     .               s2rfac(ilrsegs,iconx)
               end do
            end do
         end do
      end do      



      if ( debug .eq. 1 ) then
      !{
         print*, 'Debug 1'
         print*, 'nlrsegs = ',nlrsegs
         print*, 'nconx   = ',nconx

         do ilrsegs = 1,nlrsegs
            do iconx = 1,nconx
               do iyear = sdate(1),edate(1)
                 print*, lrsegs(ilrsegs),',',C_conx(iconx),',',iyear,
     .            ',',(lrsegloadinp(ilrsegs,iconx,iyear,imon),imon=1,12)
               end do
            end do
         end do
      !}
      end if







      !! >> INTERPOLATE
      do ilrsegs = 1,nlrsegs
      !{
         do iconx = 1,nconx
         !{
            do iyear = sdate(1),edate(1)
            !{
               imon = 7
               if (lrsegloadinp(ilrsegs,iconx,iyear,imon).lt.0) then
               !{
                  indx1 = iyear
                  do while ( indx1 .ge. sdate(1) )
                     if (lrsegloadinp(ilrsegs,iconx,indx1,imon).ge.0)
     .                  exit
                     indx1 = indx1 - 1
                  end do

                  indx2 = iyear
                  do while ( indx2 .le. edate(1) )
                     if (lrsegloadinp(ilrsegs,iconx,indx2,imon).ge.0)
     .                  exit
                     indx2 = indx2 + 1
                  end do
            
                  print*,'Interpolate ',iyear,' -> ',indx1,' - ',indx2

                  do imon = 1,12
                  !{
                     lrsegloadinp(ilrsegs,iconx,iyear,imon) = 
     .                  lrsegloadinp(ilrsegs,iconx,indx1,imon) + 
     .                  (iyear-indx1) * 
     .                  ( ( lrsegloadinp(ilrsegs,iconx,indx2,imon) -
     .                      lrsegloadinp(ilrsegs,iconx,indx1,imon) ) /
     .                  (indx2-indx1) )
                  !}
                  end do
               !}
               end if
            !}
            end do
         !}
         end do
      !}
      end do

      if ( debug .eq. 1 ) then
      !{
         print*, 'Debug 2'
         print*, 'nlrsegs = ',nlrsegs
         print*, 'nconx   = ',nconx

         do ilrsegs = 1,nlrsegs
            do iconx = 1,nconx
               do iyear = sdate(1),edate(1)
                 print*, lrsegs(ilrsegs),',',C_conx(iconx),',',iyear,
     .            ',',(lrsegloadinp(ilrsegs,iconx,iyear,imon),imon=1,12)
               end do
            end do
         end do
      !}
      end if








      !! >> Calculate DSN Data
      do ilrsegs = 1,nlrsegs
      !{
         do idsn = 1,ndsns
         !{
            do icon = 1,ncons(idsn)
            !{
               do iconx = 1,nconx
                  conxfound = .false.
                  if ( C_conx(iconx) .eq. inpcon(idsn,icon) ) then
                     conxfound = .true.
                     exit
                  end if
               end do
               if ( conxfound .eqv. .true. ) then
               !{
                  factor = confac(idsn,icon)
                  if ( factor .lt. 0 )
     .              call dynamic_factor(lrsegs(ilrsegs)(:6),factor)
                  do iyear = sdate(1),edate(1)
                  !{
                     do imon = 1,12
                     !{
                       lrsegloadout(ilrsegs,idsn,iyear,imon) =
     .                   lrsegloadout(ilrsegs,idsn,iyear,imon) +
     .                   lrsegloadinp(ilrsegs,iconx,iyear,imon) * factor
                     !}
                     end do
                  !}
                  end do
               !}
               end if
            !}
            end do
         !}
         end do
      !}
      end do







      !! >> ESTIMATE BOD AND AJUST REFRACTORY N/P

      call getdsn(outcon,ndsns,'BODR',ibodr)
      call getdsn(outcon,ndsns,'RONR',ironr)
      call getdsn(outcon,ndsns,'ROPR',iropr)
      if (debug.eq.1) print*, 'BODR DSN = ',ibodr
      if (debug.eq.1) print*, 'RONR DSN = ',ironr
      if (debug.eq.1) print*, 'ROPR DSN = ',iropr



C      fnam = catdir//'iovars/'//ioscen(:lenioscen)//
C       fnam = catdir//'iovars/'//'p600'//
       fnam = tree//'config/catalog/iovars/'//'p600'//
     .          '/rpa_species_cvfactor'
C      fnam = 'rpa_species_cvfactor'
      SPECIES1 = 'PHOS'
      SPECIES2 = 'NITR'
      print*,fnam
      call SPECIES_CVFACTOR(fnam,SPECIES1,SPECIES2,CVP2N)

      SPECIES1 = 'NITR'
      SPECIES2 = 'BODX'
      call SPECIES_CVFACTOR(fnam,SPECIES1,SPECIES2,CVN2BOD)

      if (debug.eq.1) print*,'CVP2N   = ', CVP2N
      if (debug.eq.1) print*,'CVN2BOD = ', CVN2BOD

      do ilrsegs = 1,nlrsegs
         do iyear = sdate(1),edate(1)
            do imon = 1,12
               if ( lrsegloadout(ilrsegs,ironr,iyear,imon) .lt.
     .           CVP2N * lrsegloadout(ilrsegs,iropr,iyear,imon) ) then

                  lrsegloadout(ilrsegs,ibodr,iyear,imon) = 
     .                       CVN2BOD
     .                     * lrsegloadout(ilrsegs,ironr,iyear,imon)
                  lrsegloadout(ilrsegs,iropr,iyear,imon) = 
     .                       lrsegloadout(ilrsegs,iropr,iyear,imon)
     .                     - ( lrsegloadout(ilrsegs,ironr,iyear,imon)
     .                     / CVP2N )
                  lrsegloadout(ilrsegs,ironr,iyear,imon) = 0.0D0
               else
                  lrsegloadout(ilrsegs,ibodr,iyear,imon) =
     .                       CVN2BOD 
     .                     * lrsegloadout(ilrsegs,iropr,iyear,imon)
     .                     * CVP2N
                  lrsegloadout(ilrsegs,ironr,iyear,imon) =
     .                       lrsegloadout(ilrsegs,ironr,iyear,imon)
     .                     - ( lrsegloadout(ilrsegs,iropr,iyear,imon)
     .                     * CVP2N )
                  lrsegloadout(ilrsegs,iropr,iyear,imon) = 0.0D0
               end if
            end do
         end do
      end do

C      return


      if ( debug .eq. 1 ) then
      !{
         print*, 'Debug 3' 
         print*, 'nlrsegs = ',nlrsegs
         print*, 'ndsns   = ',ndsns
         
         do ilrsegs = 1,nlrsegs
            do idsn = 1,ndsns
               do iyear = sdate(1),edate(1)
                 print*, lrsegs(ilrsegs),',',outcon(idsn),',',iyear,
     .            ',',(lrsegloadout(ilrsegs,idsn,iyear,imon),imon=1,12)
               end do
            end do
         end do
      !}
      end if





      ndays = julian(sdate(1),1,1,edate(1),12,31)




      !! >> WRITE WDMS AND LOAD SUMMARY FILE
      call lencl(rpascen,lenrpascen)
      wdmfnam = dummyWDMname
C      wdmfnam = '../../../../config/blank_wdm/dummy.wdm'
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)
      if (err.ne.0) go to 791


      fnam = ScenDatDir//'river/rpaload/'//
C      fnam = '../../../../input/scenario/river/rpaload/'//
     .       rpascen(:lenrpascen)//'/summary_rpaloads'//'.csv'
      open (dfile+2,file=fnam,status='unknown',iostat=err)
      if ( err.ne.0 ) go to 991
      write(dfile+2,1000) 'lseg,rseg',(outcon(idsn),idsn=1,ndsns)


      do ilrsegs = 1,nlrsegs
      !{

         wdmfnam = ScenDatDir//'river/rpaload/'//
C         wdmfnam = '../../../../input/scenario/river/rpaload/'//
     .       rpascen(:lenrpascen)//'/'//rpa(:lenrpa)//'_'//
     .       lrsegs(ilrsegs)(:6)//'_to_'//lrsegs(ilrsegs)(7:19)//'.wdm'
         command = 'cp '//tree//'config/blank_wdm/blank_rpa.wdm '//
C         command = 'cp '//tree//'../config/blank_wdm/blank_rpa.wdm '//
     .            wdmfnam
         call system(command)

         call wdbopnlong(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 792

         do idsn = 1,ndsns
         !{
            ndays = 0
            load(idsn) = 0
            do iyear = sdate(1),edate(1)
               do imon = 1,12
                  daysinmonth = ndaysinmonth(iyear,imon)
                  do iday = 1,daysinmonth
                     ndays = ndays + 1
                     dvals(ndays) =lrsegloadout(ilrsegs,idsn,iyear,imon)
     .                           / daysinmonth
                  end do
                  load(idsn) = load(idsn)
     .                        + lrsegloadout(ilrsegs,idsn,iyear,imon)
               end do
            end do

            print*,'Writing... ',outcon(idsn),' as ',outdsn(idsn)
            call putdailydsn(wdmfil,sdate,edate,
     .                        outdsn(idsn),ndays,dvals)
         !}
         end do

         write(dfile+2,1234) 
     .            lrsegs(ilrsegs)(:6),',',lrsegs(ilrsegs)(7:19),
     .            (load(idsn),idsn=1,ndsns)

         call wdflcl(wdmfil,err)
         if ( err .ne. 0 ) go to 793
      !}
      end do

      close (dfile+2)

      return

1000  format(A,9(',',A))
1234  format(A6,A,A13,9(',',E9.4))








891   report(1) = 'Problem: Unable to open file: '
      report(2) = fnam
      report(3) = ' '
      go to 999

892   report(1) = 'Problem reading line'
      report(2) = fnam
      report(3) = dline
      go to 999



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

995   report(1) = 'Problem: parsing file: '
      report(2) = fnam
      go to 999

996   report(1) = 'Problem nutx not found in rpa_to_wdm'
      report(2) = nutx
      go to 999

791   report(1) = 'WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

792   report(1) = 'Blank RPA WDM file must exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

793   report(1) = 'Problem: Error Closing WDM File'
      report(2) = wdmfnam
      write(report(3),*) 'Error = ',err
      go to 999


999   call stopreport(report)

      end



      subroutine getdsn(outcon,ndsns,C4_con,idsn)

         include 'rpa.inc'

         dsnfound = .false.
         do idsn = 1,ndsns
            if ( outcon(idsn) .eq. C4_con ) then
               dsnfound = .true.
               exit
            end if
         end do
         if ( dsnfound .eqv. .false. ) go to 601

         return

601      report(1) = 'Problem DSN not found'
         report(2) = C4_con
         write(report(3),*) (outcon(idsn),idsn=1,ndsns)
         goto 699

699      call stopreport(report)

      end












      SUBROUTINE SPECIES_CVFACTOR(filename,tspecies1,tspecies2,factor)
      !{
         implicit none
         include '../../lib/inc/standard.inc'

         character*200 filename
         character*4   tspecies1,tspecies2
         real          factor

         logical       comment
         external      comment

         logical       found

         found = .false.

         open(dfile,file=filename,status='old',iostat=err)
         if (err.ne.0) go to 991

         do
         !{
            read(dfile,'(a100)',err=992,end=410) line
            call d2x(line,last)
            if (comment(line)) cycle
            if (line(:20).eq.'                    ') cycle
c            print*,line(26:29),'<>',C4_tStr
            if (line(1:4).eq.tspecies1.and.line(10:13).eq.tspecies2)then
            !{
               read(line(19:),*) factor
               found = .true.
               goto 410
            !}
            end if
         !}
         end do
         if ( found .eqv. .false. ) goto 993

410      close (dfile)

         return

991      report(1) = 'Error 991c Problem opening file:'
         report(2) = fnam
         write(report(3),*)'iostat error = ',err
         go to 999

992      report(1) = 'Error 992c Problem reading file: near line:'
         report(2) = fnam
         report(3) = line
         go to 999

993      report(1) = 'Error 993c Problem factor not found '
         report(2) = tspecies1
         report(3) = tspecies2
         go to 999

999   call stopreport(report)

      !}
      END


      subroutine dynamic_factor(lseg, factor)

         include 'rpa.inc'

         character*6 ilseg
         real        ifactor

         print*, 'dynamic_factor for lseg ',lseg,' and ',factor

         if ( factor .eq. -1 ) then
            fnam = catdir//'iovars/'//'p600/'//'variable_901_factors/'//
     .              'SEDM_to_SAND.csv'
         end if

         if ( factor .eq. -2 ) then
            fnam = catdir//'iovars/'//'p600/'//'variable_901_factors/'//
     .              'SEDM_to_SILT.csv'
         end if

         if ( factor .eq. -3 ) then
            fnam = catdir//'iovars/'//'p600/'//'variable_901_factors/'//
     .              'SEDM_to_CLAY.csv'
         end if


         open (dfile+3,file=fnam,status='old',iostat=err)
         if ( err.ne.0 ) go to 701


         read(dfile+3,*,err=702,end=703) dline !header 
         do while ( .true. )
            read(dfile+3,*,err=702,end=703) ilseg,ifactor
            if ( ilseg .eq. lseg ) exit
         end do

         close ( dfile+3 )

         factor = ifactor

         print*, 'dynamic_factor for lseg ',lseg,' and ',factor

         return

701      report(1) = 'Problem opening file'
         report(2) = fnam
         go to 799

702      report(1) = 'Problem unable to parse file'
         go to 799

703      report(1) = 'Problem lseg not found on file'
         report(2) = lseg
         go to 799


799      call stopreport(report)
      END