************************************************************************
**  The purpose of this program is to prepare the read/write wdm for  **
**    any stream.  The major steps are:                               **
**      1.  Only run if there are upstream segments                   **
**      2.  Determine if there are upstream segments that are not run **
**      3.  Add upstream segments to the edge-of-stream loads from    **
**            the current segment                                     **
**      4.  Store in ./wdm/river/$scen/stream/???.wdm                 **
************************************************************************

      implicit none
      include 'stream_wdm.inc'

      integer asdate(ndate),aedate(ndate) ! test dates for checking wdm

      integer wdmfil
      parameter (wdmfil=12)         ! file number for wdm

      integer n,RvarIn,RvarOut         ! indices

      integer upNexits,uplakeflag ! check exits of upstream
      character*1 resflag,upresflag
      integer timestep

*************************************************************************      
      read*,rscen,rseg     
      print*,'Making stream wdm for segment ', rseg
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      call getupstream(rseg,rscen,lenrscen,
     O                 upstream,nup)
      do n = 1,nup
        call lencl(upstream(n),lenup(n))
      end do
      
      if (nup.ne.0) then                      ! upstream segments exist

        wdmfnam = rseg(:lenrseg)//'.wdm'
        call wdbopnlong(wdmfil,wdmfnam,0,err)     ! open main wdm read/write
        if (err .ne. 0) go to 991

        call readcontrol_time(rscen,lenrscen,sdate,edate)  ! get start and stop

        call readcontrol_modules(rscen,lenrscen,  ! get active modules
     O                           modules,nmod)
       
        do n = 1,nup                           ! loop over upstream segments

          print*,' ' 
          call ttyput(' upstream segment ')
          call ttyput(upstream(n))
 
          call readcontrol_Rparamscen(
     I                                rscen,lenrscen,
     O                                paramscen)
          call lencl(paramscen,lenparamscen)

          call getrflags(   ! determine if this upstream is a reservoir
     I                   paramscen,lenparamscen,upstream(n),
     O                   upNexits,uplakeflag,upresflag,timestep)

          call Rmasslink(
     I                   ioscen,lenioscen,
     I                   upNexits,modules,nmod,
     O                   nRvarIn, RdsnIn, RnameIn,
     O                   nRvarOut,RdsnOut,RnameOut)    ! POPULATE nRvar, Rdsn, Rname

          wdmfnam = outwdmdir//'river/'//rscen(:lenrscen)//
     .              '/stream/'//upstream(n)(:lenup(n))//'.wdm'
          call wdbopnlong(wdmfil+n,wdmfnam,1,err)     ! open upstream wdm read only
          if (err .eq. -2) go to 992
          if (err .ne. 0) go to 993

          call wtdate(wdmfil+n,1,RdsnOut(1),2,asdate,aedate,err)    ! tests the date
          if (err .ne. 0) go to 994

          do RvarIn = 1,nRvarIn

            call ttyput(' ')
            call ttyput(RnameIn(RvarIn))

            do RvarOut = 1,nRvarOut

              if (RnameIn(RvarIn).eq.RnameOut(RvarOut)) then

                if (RvarIn.eq.1) call checkflow(wdmfnam,wdmfil,n,
     .                                RdsnOut(RvarOut),sdate,edate)

                call addRwdm(wdmfnam,wdmfil,n,RdsnIn(RvarIn),
     .                       RdsnOut(RvarOut),sdate,edate)

              end if

            end do

          end do

          call wdflcl(wdmfil+n,err)
          if (err.ne.0) go to 995

        end do            ! loop over stream segments

        call wdflc1(wdmfil,err)
        if (err.ne.0) go to 996

      else              ! no upstream segments

        print*,'No Upstream Segments for ',rseg

      end if            ! end if upstream segments exist

      print*,' '

      return

************************ error reporting

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

994   report(1) = 'Problem getting dates from open wdm '
      report(2) = wdmfnam
      call wdmerr(err,report(3))
      go to 999

995   report(1) = ' Could not Close upstream wdm'
      report(2) = '  for segment '//upstream(n)
      report(3) = wdmfnam
      go to 999

996   report(1) = ' Could not close current wdm'
      report(2) = '  for segment '//rseg
      report(3) = wdmfnam
      go to 999

999   call stopreport(report)

      end
