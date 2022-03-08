************************************************************************
** Program to populate eos wdms with river input variables            **
**  wdms are stored in ./wdm/river/$scen/eos/$seg.wdm                 **
**    general strategy:                                               **
**      Run the program once for each river or bfl subseg             **
**      For each land segment in the river                            **
**         for each land use in the segment                           **
**           determine the time series of river input by:             **
**             extracting relevant dsns                               **
**             multiply by the etm binary files                       **
**           add to the appropriate river variable                    **
**      sum over all segments                                         **
**      store in river wdm                                            **
**      write appropriate output files (eos, wqm)                     **
************************************************************************
      subroutine readpair(rscen,tlseg,trseg,year1,year2,
     I              LandScen,nRvar,Rdsn,Rname,nLvar,Ldsn,Lname,Lfactor,
     O              pairq)  ! get the data
      implicit none

      include 'transfer_wdm.inc'

      character*13 trseg
      character*6 tlseg
      real pairq(-5:366,year1:year2)
      integer ny              ! year of infomation to get

      real lufac(ndaymax,nlu)           ! variables to hold the etm file
      real bmpfac(ndaymax,nbcon,nlu)
      real tranfac(nbcon,nlu) 

      real hvaltemp(ndaymax*24,maxRvar)   ! hourly values to river

      character*25 LStest             ! test the etm file

      integer jday,jdaytot          ! julian day determined from hours
      real fac1,fac2,fac3,fac4, fac5

      integer i, ii, l, Rvar, Lvar, nv, nlconi, nb, nbi, hour  ! indices
      integer wdmlnd,etmfil
       
      parameter (etmfil=dfile+1)
      parameter (wdmlnd=etmfil+1)

      integer sdate(ndate),edate(ndate)  ! start end dates wdm format

      logical dothislu                ! decide to use this land use

      integer ndaysinyear,ndays,nd
      external ndaysinyear

************ END DECLARATION ******************************************


********* GET START AND END DATE IN WDM FORMAT
      sdate(1) = year1
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = year2
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      ndays = julian(sdate(1),sdate(2),sdate(3),
     .               edate(1),edate(2),edate(3))

 	print*,11
********** wind back sdate to -5
      do nd = 0,-5,-1
        call yesterday(sdate(1),sdate(2),sdate(3))
        ndays = ndays + 1
      end do


      do Rvar = 1,maxRvar     ! initialize temp variable
        do nv = 1,ndays*24
          hvaltemp(nv,Rvar) = 0.
        end do
      end do

 	print*,12
*********  OPEN AND READ ETM FILE
      call lencl(rscen,lenrscen)
      call lencl(tlseg,lenlseg)
      call lencl(trseg,lenrseg)
	print*,120
      fnam = tree//'run/river/'//rscen(:lenrscen)//'/R'//
     .         trseg(:lenrseg)//'L'//tlseg(:lenlseg)//'.etm'
      open(etmfil,file=fnam,status='old',form='unformatted',iostat=err)
      if (err.ne.0) go to 991
	print*,121
      read (etmfil) lufac,bmpfac,tranfac,LStest
	print*,122
      if (LStest.ne.LandScen(1)) go to 997
      close(etmfil)

 	print*,13,sdate,edate
      do l = 1,nlu   ! loop over land uses
         
        dothislu = .false.     !check whether to process this land use
        jday = 1          ! do not process if water or zero land use
        do while (jday.lt.ndaymax.and..not.dothislu)
          if (lufac(jday,l).gt.0.001) dothislu = .true.
          jday = jday + 1
        end do
        if (luname(l).eq.'wat') dothislu = .false.

        if (.not.dothislu) cycle

************ READY TO OPEN WDM FILE
        call lencl(LandScen(l),lenls)
        wdmfnam = tree//'wdm/land/'//luname(l)//'/'//
     .            LandScen(l)(:lenls)//'/'//luname(l)//
     .            tlseg(:lenlseg)//'.wdm'
        call wdbopn(wdmlnd,wdmfnam,1,err)     ! open land read only
        if (err .ne. 0) go to 998

        do Rvar = 1,nRvar  ! loop over river variables
	print*,Rvar,' ',Rname(Rvar)

          if (nLvar(Rvar,l).le.0) cycle

          do Lvar = 1,nLvar(Rvar,l)  ! loop over land variables

	print*,luname(l),' ',Lvar,' ',Lname(Rvar,l,Lvar),' ',Ldsn(Rvar,l,Lvar)
************************************************************************
!              for each land variable that goes to a river variable
!              1.  get the hourly variable amount
!              2.  find out what type of variable it is (e.g. tn tp)
!              3.  multiply by land use
!              4.  multiply by appropriate bmp factor
!              5.  multiply by the land to water factor
!              6.  add to river variable temporary variable
************************************************************************

 	print*,13,sdate,edate
            call gethourdsn(wdmlnd,sdate,edate,
     I                       Ldsn(Rvar,l,Lvar),
     O                       nvals,hval)                     !(1)

            do nlconi = 1,nlcon                          !(2)
              if (Lname(Rvar,l,Lvar).eq.lconname(nlconi)) then
                do nbi = 1,nbcon
                  if(lconsurrogate(nlconi).eq.bconname(nbi))nb=nbi
                end do
              end if 
            end do
 
            jdaytot = nvals/24
            if (jdaytot.ne.ndays) go to 992
            nv = 0
            fac1 = Lfactor(Rvar,l,Lvar)*tranfac(nb,l)
            do jday = 1,jdaytot
              fac2 = fac1 * lufac(jday,l)*bmpfac(jday,nb,l)

              do hour = 1,24
                nv = nv + 1
                hvaltemp(nv,Rvar)=fac2*hval(nv)+hvaltemp(nv,Rvar) !(6)
              end do
            end do          ! end loop over days

          end do           ! end loop over land variables

            
        end do            ! end loop over river variables in land use

        call wdflc1 (wdmlnd,err)
        if (err.ne.0) go to 996

      end do            ! end loop over land uses in segment

      do jday = -5,ndaysinyear(ny)
        pairq(jday) = 0.0
      end do

      nv = 0
      do Rvar = 1,nRvar  ! store water flows
        if (Rname(Rvar).ne.'WATR') cycle
        do jday = -5,ndaysinyear(ny)
          do hour = 1,24
            pairq(jday) = pairq(jday) + hvaltemp(nv,Rvar) 
          end do
        end do
      end do
          
      return

********************************* ERROR SPACE **********************************
991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'problem in wdm, difference between '
      report(2) = 'expected days and read days'
      write(report(3),*)'exp ',ndays,' read ',jdaytot
      go to 999

996   report(1) = 'Error: closing wdm = '
      write(report(1)(22:24),'(i3)')err
      report(2) = ' '
      report(3) = ' '
      go to 999

997   report(1) = '  etm file not correct format'
      report(2) = ' expected the string '//
     .         LandScen(1)//' after bmpfac variable'
      report(3) = '  got instead the string '//LStest
      go to 999

998   if (err.lt.0) then
        report(1) = 'Error: opening wdm= '
        write(report(1)(22:24),'(i3)')err
        report(2) = wdmfnam
      else
        report(1) = wdmfnam
        report(2) = ' is not a wdm file'
      end if
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

