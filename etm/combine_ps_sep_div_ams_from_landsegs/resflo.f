************************************************************************
**  for reservoirs with 4 exits, this program will read the upstream  **
**    river inflow, process it through the specified set of rules     **
**    and store the output in a wdm                                   **
************************************************************************
      subroutine resflo(resflag,rscen,rseg,wdmrch,sdate,edate)
      implicit none
      include 'res.inc'

      real resin(ndaymax),resout(ndaymax)  ! daily inflow and outflow

      integer wdmrch  ! file number for reach wdm
      integer wdmuprch  ! file number for upstream reach wdm
      integer sdate(ndate),edate(ndate)  ! start and end dates

      integer n,nv,nd,ndays,ndi,RvarOut,nh    ! indices
      character*1 upresflag

*************** END DECLARATIONS   *************************************

      print*,' '
      print*,'Making outflow file for segment ',rseg

      wdmuprch = wdmrch + 2   ! set file number for reading wdms

      call lencl(rscen,lenrscen)
      call getupstream(rseg,rscen,lenrscen,
     O                 upstream,nup)   ! find upstream segments
      do n = 1,nup
        call lencl(upstream(n),lenup(n))
      end do

      do nv = 1,ndaymax
        resin(nv) = 0.0
      end do

      call lencl(rscen,lenrscen)
      call readcontrol_modules(rscen,lenrscen,  ! get active modules
     O                         modules,nmod)
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      call ttyput(' upstream segment:')
      do n = 1,nup          ! loop over upstream segments

        call ttyput(' ')
        call ttyput(upstream(n))

        call readcontrol_Rparamscen(
     I                              rscen,lenrscen,
     O                              paramscen)
        call lencl(paramscen,lenparamscen)
        call getrflags(   ! determine exits for this upstream segment
     I                 paramscen,lenparamscen,upstream(n),
     O                 upNexits,uplakeflag,upresflag,timestep) 

        call Rmasslink(
     I                 ioscen,lenioscen,upNexits,modules,nmod,
     O                 nRvarIn, RdsnIn, RnameIn,    ! POPULATE nRvar,
     O                 nRvarOut,RdsnOut,RnameOut)   ! Rdsn, Rname

        wdmfnam = outwdmdir//'river/'//rscen(:lenrscen)//
     .            '/stream/'//upstream(n)(:lenup(n))//'.wdm'
        call wdbopnlong(wdmuprch,wdmfnam,1,err)   ! open upstream wdm
        if (err .eq. -2) go to 992
        if (err .ne. 0) go to 993

        do RvarOut = 1,nRvarOut

          if (RnameOut(RvarOut).eq.'WATR') then
            call ttyput(' ')
            call ttyput(RnameOut(RvarOut))

            call gethourdsn(wdmuprch,sdate,edate,RdsnOut(RvarOut),
     O                      nvals,hval)
            ndays = nvals/24
            do nd = 1,ndays
              do nh = 1,24
                resin(nd) = resin(nd) + hval((nd-1)*24+nh)  ! total acre feet
              end do
            end do

          end if  ! end if a water dsn

        end do  ! end loop over variables

        call wdflcl(wdmuprch,err)
        if (err.ne.0) go to 995

      end do            ! loop over upstream segments

      do nd = 1,ndays
        resin(nd) = resin(nd) * 12.10 / 24.0  ! convert to cfs
      end do

      nvals = ndays
      call resconvert(                 ! convert input to output
     I                rscen,rseg,resin,
     O                resout,
     I                nvals,sdate(1),sdate(2),sdate(3))

      call findopen(n)
      open(n,file='resin_out.csv',status='unknown')
      write(n,*,err=951) ' day, in, out'
      do nd = 1,ndays   
C        resout(nd) = resout(nd) ! no conversion, EXTNL OUTDGT in cfs
        write(n,*,err=951)nd,',',resin(nd),',',resout(nd)
      end do
      close(n)

      call resoutdsn(
     I               ioscen,lenioscen,
     O               dsn)  ! get output dsn
      call putdailydsn(wdmrch,sdate,edate,dsn,ndays,resout)

      return
************************ error reporting
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem with opening wdm for main segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

992   report(1) = 'Problem opening wdm for upstream segment '//
     .            upstream(n)
      report(2) = '  WDM does not exist'
      report(3) = ' upstream segment probably not run'
      go to 999

993   report(1) = 'Problem opening wdm for upstream segment '//
     .            upstream(n)
      report(2) = '  Error = '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' probably need to rerun'//upstream(n)
      go to 999

995   report(1) = ' Could not Close upstream wdm'
      report(2) = '  for segment '//upstream(n)
      report(3) = wdmfnam
      go to 999

999   call stopreport(report)

      end


