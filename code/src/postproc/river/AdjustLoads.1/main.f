
      implicit none

      include 'AdjustLoads.inc'

      character*4 load
      real        factor
      real        before, after

      integer     asdate(ndate),aedate(ndate)

      integer     wdmfil
      parameter   (wdmfil=12)           ! file number for wdm

      logical     founddouble

      character*1 resflag

      integer     Nexits,idummy,timestep

      integer     iRvar,iLoad,iCon,iVal



      read(*,*) rscen,rseg,load,factor
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)


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

      print*,nRvar
      print*,Rdsn
      print*,Rname
      print*,asdate(1)
      print*,aedate(1)


      call loadinfo(rscen,lenrscen,nRvar,Rname,
     .              nloads,loadname,unit,ncons,con,confactor)


      do iLoad = 1,nloads
        if ( loadname(iLoad) == load ) then
          do iRvar = 1,nRvar
            do iCon = 1,ncons(iLoad)
              if ( Rname(iRvar) .eq. con(iLoad,iCon) ) then
                dsn = Rdsn(iRvar)
                print*,Rname(iRvar),'<->',con(iLoad,iCon),'<->',dsn
                call gethourdsn(wdmfil,asdate,aedate,dsn,nvals,hval)
                before = 0
                after  = 0
                do iVal = 1,nvals
                   before     = before + hval(iVal)
                   hval(iVal) = hval(iVal) * factor
                   after      = after  + hval(iVal)
                end do
                call puthourdsn(wdmfil,asdate,aedate,dsn,nvals,hval)
                print*,'Loads ',before,' x ',factor,' -> ',after
              end if
            end do
          end do
        end if
      end do

      

 
      call wdflcl(wdmfil,err)

      stop

991   report(1) = 'Problem with opening wdm for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

999   call stopreport(report)      

      end
