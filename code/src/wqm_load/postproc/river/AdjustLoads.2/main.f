
      implicit none

      include 'reservoir_type_II.inc'

      character*4 load
      character*4 loads(5)
      data loads  /'BODA','PHYT','TOTN','TOTP','TSSX'/
      double precision        facQ,facN,facP,facS
      double precision        factor,tconfactor

      real        before, rhs, after

      integer     asdate(ndate),aedate(ndate)
      integer     dsnL, dsnR

      integer     wdmfil
      parameter   (wdmfil=12)           ! file number for wdm

      logical     founddouble

      character*1 resflag

      integer     Nexits,idummy,timestep

      integer     iRvar,iLoad,iCon,iCon1,iVal

      real        hvalL(ndaymax*24), hvalR(ndaymax*24)

      integer     outflag !output/river/res_eff/$rsecn/$rseg.out

      read(*,*) rscen,rseg,outflag,facQ,facN,facP,facS
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      if (outflag.eq.1) then
         fnam = outdir//'river/res_eff/'//rscen(:lenrscen)//'/'//
     .          rseg(:lenrseg)//'_AdjLoad.out'
         open (dfile+2,file=fnam,status='unknown',iostat=err)
         if (err.ne.0) go to 990
      end if

      !call gettransport_res_eff(rscen,rseg,facQ,facN,facP,facS)
      print*,'Res Passthru Effs = ',facQ,facN,facP,facS
      if (outflag.eq.1) write(dfile+2,*)
     .   'Res Passthru Effs = ',facQ,facN,facP,facS
      if ( facQ.eq.1 .and. facN.eq.1 .and. 
     .     facP.eq.1 .and. facS.eq.1 ) then
         print*,'All Res Passthru Effs are 1 ... nothing to do ...'
         if (outflag.eq.1) write(dfile+2,*)
     .          'All Res Passthru Effs are 1 ... nothing to do ...'
         return
      end if


      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)
      call getrflags(
     I               paramscen,lenparamscen,rseg,
     O               Nexits,idummy,resflag,timestep)


      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+5,wdmfnam,0,err)


      wdmfnam = rseg(:lenrseg)//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)                           ! open main wdm read/write
      if (err .ne. 0) go to 991


      call WDMinfo(rscen,Nexits,nRvar,Rdsn,Rname)!POPULATE nRvar,Rdsn,Rname

      call wtdate(wdmfil,1,Rdsn(1),2,asdate,aedate,err)

      print*,'nVar-> ',nRvar
      print*,'DSNs-> ',(Rdsn(iRvar),iRvar=1,nRvar)
      print*,'RVar-> ',(Rname(iRvar),' ',iRvar=1,nRvar)
      print*,asdate(1)
      print*,aedate(1)


      call loadinfo(rscen,lenrscen,nRvar,Rname,
     .              nloads,loadname,unit,ncons,con,confactor)


      do iLoad = 1,nloads
c         call get_factor(loadname(iLoad),facQ,facN,facP,facS,
c     .                                   factorL,factorR)
c         do iRvar = 1,nRvar
c            do iCon = 1,ncons(iLoad)
         print*,''
         print*,'------------------------------------------------------'
         if (outflag.eq.1) write(dfile+2,*)
     .      ''
         if (outflag.eq.1) write(dfile+2,*)
     .      '------------------------------------------------------'
         do iCon = 1,ncons(iLoad)
            do iRvar = 1,nRvar
              if ( Rname(iRvar) .eq. con(iLoad,iCon) ) then
                if (iCon .eq. 1) iCon1 = iRvar;
                call get_factor(loadname(iLoad),Rname(iRvar),
     .                             facQ,facN,facP,facS,factor)
                print*,''
                print*,'Table:', loadname(iLoad),', Variable:',
     .              Rname(iRvar),' -> PassThru Eff =',factor
                if (outflag.eq.1) write(dfile+2,*)
     .             ''
                if (outflag.eq.1) write(dfile+2,*)
     .             'Table:', loadname(iLoad),', Variable:',
     .              Rname(iRvar),' -> PassThru Eff =',factor
c                dsnL = Rdsn(iRvar)
c                print*,Rname(iRvar),'<->',con(iLoad,iCon),'<->',dsn
c                call gethourdsn(wdmfil,asdate,aedate,dsn,nvals,hval)
                if ( confactor(iLoad,iCon) .gt. 0 ) then
                  dsnL = Rdsn(iRvar)
                  dsnR = Rdsn(iRvar)
                  tconfactor = confactor(iLoad,iCon)
                else
                  dsnL = Rdsn(iRvar)
                  dsnR = Rdsn(iCon1)
                  tconfactor = -1.0 * confactor(iLoad,iCon)
                  if (factor .gt. 0) factor = 1 + factor
                end if

                call gethourdsn(wdmfil,asdate,aedate,dsnL,nvals,hvalL)
                call gethourdsn(wdmfil,asdate,aedate,dsnR,nvals,hvalR)

                before = 0
                rhs    = 0
                after  = 0
                if ( factor .gt. 0 .and. tconfactor .gt. 0 ) then
                  do iVal = 1,nvals
                      before      = before + hvalL(iVal)
                      rhs         = rhs    + hvalR(iVal)
                      hvalL(iVal) = hvalL(iVal) 
     .                           + hvalR(iVal) * (factor-1) * tconfactor
                      after       = after  + hvalL(iVal)
                  end do
                  call puthourdsn(wdmfil,asdate,aedate,dsnL,nvals,hvalL)
                  print*,'Load',before,' +',rhs,' x',factor-1,
     .                  ' x',tconfactor,' =',after
                  if (outflag.eq.1) write(dfile+2,*)
     .               'Load',before,' +',rhs,' x',factor-1,
     .                  ' x',tconfactor,' =',after
                else
                  print*,'... skipping ...'
                  if (outflag.eq.1) write(dfile+2,*)
     .               '... skipping ...'
                end if
              end if
            end do
          end do
      end do

      

 
      call wdflcl(wdmfil,err)

      stop

990   report(1) = 'Problem with opening file '
      report(2) = fnam
      report(3) = ' '
      go to 999

991   report(1) = 'Problem with opening wdm for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

999   call stopreport(report)      

      end











      subroutine stopit(fnam,tabnam)
      include '../../../lib/inc/standard.inc'
      character*(*) tabnam

      report(1) = 'Problem reading control file for table'
      report(2) = fnam
      report(3) = tabnam
      call stopreport(report)

      end




      subroutine get_factor(loadL,loadR,facQ,facN,facP,facS,factor)

      include '../../../lib/inc/standard.inc'

      character*4   loadL, loadR
      double precision          facQ, facN, facP, facS
      double precision          factor

      factor = -9.9D0

      if (loadL .eq. 'TOTN') then
         factor = facN
      end if

      if (loadL .eq. 'TOTP') then
         factor = facP
      end if

      if (loadL .eq. 'TSED') then
         factor = facS
      end if

      if (loadL .eq. 'TORC') then
         factor = facP
      end if

      if (loadL .eq. 'BODA') then
         factor = min(facN,facP)
         if (loadR .eq. 'RORN') factor = facN-factor
         if (loadR .eq. 'RORP') factor = facP-factor
         if (loadR .eq. 'TORC') factor = facP-factor
      end if

      if (loadL .eq. 'PHYT') then
         factor = min(min(facN,facP),facS)
         if (loadR .eq. 'RORN') factor = facN-factor
         if (loadR .eq. 'RORP') factor = facP-factor
         if (loadR .eq. 'TORC') factor = facP-factor
      end if

      if (factor .eq. -9.9) go to 900

c      print*,''
c      print*,'Table:', loadL,', Variable:',loadR,
c     .       ' -> PassThru Eff =',factor

      return

900   report(1) = 'load variable not defined in the function get_factor'
      report(2) = 'load variable '//loadL//','//loadR
      report(3) = ''
      call stopreport(report)

      end
