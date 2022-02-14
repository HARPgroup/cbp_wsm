************************************************************************
**  The purpose of this program is to find monthly load totals        **
**    The Strategy                                                    **
**      1.  get segment and scenario                                  **
**      2.  get info from catalog file                                **
**      3.  read water and store it to find concentrations            **
**      4.  read and write concentrations for each variable           **
************************************************************************
      subroutine RiverLoads(rscen,rseg,wdmfil,Nexits,OutInFlag,
     .   TimeInterval)

      implicit none
      include 'RiverLoads.inc'

      integer asdate(ndate),aedate(ndate)           ! test dates for checking wdm

      integer wdmfil
   
      character*100 pfname

      integer i,np,nc,j,n,Rvar,hour,oldyear,oldmonth,oldday,oldhour ! indices

      integer maxWATR,nWATRdsns    ! number of DSNs that make up the total flow (multiple exits)
      parameter (maxWATR=4)
      integer WATRdsn(maxWATR)      ! dsns for flow variables

      real flowvals(ndaymax*24)        ! flow
      real loadvals(ndaymax*24)        ! load

      real dload    ! daily load accumulator

      character*4 Tdsn

      integer Nexits,idummy

      logical found,foundall

************* END DECLARATIONS
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

c      if (TimeInterval.ne.'D' .or.
c     .    TimeInterval.ne.'M' .or.
c     .    TimeInterval.ne.'Y') then
c        print*,'PROBLEM with TimeInterval ',TimeInterval
c        go to 4922
c      end if

c      if (OutInFlag.ne.'O' .or.
c     .    OutInFlag.ne.'I') then
c        print*,'PROBLEM with OutInFlag ',OutInFlag
c        go to 4921
c      end if
      
      print*,'River Loads ',rscen(:lenrscen),' ',rseg,
     .  " OutInFlag = ",OutInFlag," TimeInterval = ", TimeInterval

      if (OutInFlag.eq.'O')call WDMinfo(rscen,Nexits,nRvar,Rdsn,Rname)! POPULATE nRvar, Rdsn, Rname 
      if (OutInFlag.eq.'I')call WDMinfoIN(rscen,Nexits,nRvar,Rdsn,Rname)

      call loadinfo(rscen,lenrscen,nRvar,Rname,
     .              nloads,loadname,unit,ncons,con,confactor)     ! POPULATE loading variables
      call readcontrol_time(rscen,lenrscen,sdate,edate)         ! get start and stop

      wdmfnam = outwdmdir//'river/'//rscen(:lenrscen)//
     .            '/stream/'//rseg(:lenrseg)//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)                           ! open main wdm read/write
      if (err .ne. 0) go to 991

      call wtdate(wdmfil,1,Rdsn(1),2,asdate,aedate,err)    ! tests the date

      if (err .ne. 0) go to 994

      nWATRdsns = 0
      do Rvar = 1,nRvar                     ! find which dsns corresponds to water
        if (Rname(Rvar).eq.'WATR') then
          nWATRdsns = nWATRdsns + 1
          if (nWATRdsns.gt.maxWATR) go to 995
          WATRdsn(nWATRdsns) = Rdsn(Rvar)
        end if
      end do

      call checkflow(wdmfnam,wdmfil,0,maxWATR,nWATRdsns,WATRdsn,
     .                     sdate,edate)               ! make sure that flow exists

      call gethourdsn(wdmfil,sdate,edate,WATRdsn(1),nvals,flowvals)         ! get nvals from water variable
      do Rvar = 2,nWATRdsns         ! get other flowvals if more than 1 water variables
        call gethourdsn(wdmfil,sdate,edate,WATRdsn(Rvar),nvals,hval)
        do i = 1,nvals
          flowvals(i) = flowvals(i) + hval(i)
        end do
      end do

      do np = 1,nloads

        call ttyput(loadname(np))
        call ttyput(' ')

        do i = 1,nvals                 ! initialize loadvals
          loadvals(i) = 0.
        end do
        foundall = .true.
        do nc = 1,ncons(np)            ! for each constituent
          found = .false.
          do Rvar = 1,nRvar            ! get the right dsn
            if (con(np,nc).eq.Rname(Rvar)) then
              found = .true.
              dsn = Rdsn(Rvar)
              call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)      
              do i = 1,nvals
                if (hval(i).gt.0.)
     .            loadvals(i) = loadvals(i) + hval(i)*confactor(np,nc) 
              end do
            end if
          end do
          if (.not.found) foundall = .false.
        end do

*****************loada
        if (foundall) then
          if(TimeInterval .eq. 'D') then
            pfname = outdir//'river/daily/'//
     .             rscen(:lenrscen)//'/'//rseg(:lenrseg)//'_'//
     .             loadname(np)//'_'//OutInFlag//'.prn'
          else if (TimeInterval .eq. 'M') then
            pfname = outdir//'river/monthly/'//
     .             rscen(:lenrscen)//'/'//rseg(:lenrseg)//'_'//
     .             loadname(np)//'_'//OutInFlag//'.prn'
          else if (TimeInterval .eq. 'Y') then
            pfname = outdir//'river/annual/'//
     .             rscen(:lenrscen)//'/'//rseg(:lenrseg)//'_'//
     .             loadname(np)//'_'//OutInFlag//'.prn'
          end if
          open (pltfil,file = pfname, status = 'unknown',iostat = err)
          if (err.ne.0) goto 992
 
           write(pltfil,'(a19,a4)',err=951)'CON ,YEAR,MO,DD,HH,',
     .       unit(np)
      
          do i = 1,6
            asdate(i) = sdate(i)
          end do

          hour = 0
          dload = 0.
          oldhour  = asdate(4)
          oldday   = asdate(3)
          oldmonth = asdate(2)
          oldyear  = asdate(1)
          call onehour(asdate(1),asdate(2),asdate(3),asdate(4))
          do i = 1,nvals

            dload = dload + loadvals(i)
            call onehour(asdate(1),asdate(2),asdate(3),asdate(4))

            if (asdate(3).ne.oldday) then
              if(TimeInterval .eq. 'D') then
                write(pltfil,1234,err=951)
     .                loadname(np),oldyear,oldmonth,oldday,oldhour,dload
                dload = 0.
              end if
              if (asdate(2).ne.oldmonth) then
                if(TimeInterval .eq. 'M') then
                   write(pltfil,1234,err=951)
     .                loadname(np),oldyear,oldmonth,oldday,oldhour,dload
                   dload = 0.
                end if
                if (asdate(1).ne.oldyear) then
                  if(TimeInterval .eq. 'Y') then
                    write(pltfil,1234,err=951)
     .                loadname(np),oldyear,oldmonth,oldday,oldhour,dload
                    dload = 0.
                  end if
                  oldyear  = asdate(1)
                end if
                oldmonth = asdate(2)
              end if
              oldday = asdate(3)
            end if
            oldhour = asdate(4)

          end do
C          write(pltfil,1234) loadname(np),asdate(1),asdate(2),dload

          close (pltfil)

        end if

      end do            ! end loop over all loads in 'rchres_out_to_daily' file
            
      print*,' '

      return

1234  format(a4,',',i6,',',i3,',',i3,',',i3,',',e16.9)

************************ error reporting
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem with opening wdm for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

992   report(1) = 'Problem opening load files for segment '//rseg
      report(2) = pfname
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

994   report(1) = 'Problem getting dates from open wdm '
      report(2) = wdmfnam
      report(3) = '  Error = '
      write(report(3)(11:13),'(i3)') err
      go to 999

995   report(1) = 'too many variables names WATR in catalog file'
      report(2) = 'increase maxWATR variable in'
      report(3) = './pp/postproc/src/river/*/main.f'
      go to 999

996   report(1) = ' Could not close current wdm'
      report(2) = '  for segment '//rseg
      report(3) = wdmfnam
      go to 999

4921  report(1) = 'OutInFlag *not* a valid flag'
      report(2) = 'OutInFlag = '//OutInFlag
      report(3) = 'OutInFlag should be [I or O]'
      go to 999

4922  report(1) = 'TimeInterval *not* a valid flag'
      report(2) = 'TimeInterval you provided = '//TimeInterval
      report(3) = 'TimeInterval should be [D or M or Y]'
      go to 999

999   call stopreport(report)

      end

