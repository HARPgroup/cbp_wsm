      implicit none
      include 'eotbmp.inc'

      logical pourpoint

      integer Nexits,idummy
      integer ieotrvar

      integer wdmfil
      parameter (wdmfil=12)
      integer ival

      integer  ndaysinmonth
      external ndaysinmonth

      read(*,*,err=997,end=998) rscen,rseg,I_DBG

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

c      print*, rscen(:lenrscen), rseg(:lenrseg)
      if(I_DBG.ge.1)
     . print*,'... start processing '//rseg(:lenrseg)

      if ( .not. pourpoint(rseg) ) then
         if(I_DBG.ge.1)
     .    print*,'... skipping ',rseg(:lenrseg),' not a pour point'
         return
      else
         open(11,file='pourpoint',status='unknown',iostat=err)
         if ( err.ne.0 ) go to 990

         write(11,*),'hello'
         close(11)
      end if

***** GET THE LIST OF RIVER SEGMENTS
      call getrsegs(rscen,lenrscen,
     O              rsegs,uniqid,dsid,uniqindex,nrsegs)
c      do iallup = 1,nrsegs
c         print*,iallup,' ',rsegs(iallup),uniqid(iallup),dsid(iallup),
c     .          uniqindex(iallup)
c      end do


***** FIND ALL UPSTREAM SEGMENTS INCLUDING THE SEGMENT
      read (rseg(5:8),'(i4)') tuniqid
      tindex = uniqindex(tuniqid)
c      print*,tuniqid,tindex,' ',rsegs(tindex)
      call FindUpstreamRsegs(
     I                         tindex,dsid,uniqid,rsegs,nrsegs,
     O                         C_allup,I_allup,nallup)
      if(I_DBG.ge.2)
     . print*,nallup,(',',C_allup(iallup),iallup=1,nallup)


***** GET THE YEARS TO PROCESS
      call readcontrol_time(rscen,lenrscen,
     O                      sdate,edate)

***** GET INFO OF EOT BMP
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)
      call getBMPcons(
     I                ioscen,lenioscen,
     O                BMPconname,nBmpCon)
      call getBMPconsEOT(
     I                ioscen,lenioscen,
     I                BMPconname,nBmpCon,
     O                C_BMPconnameEOT,I_BMPconnameEOT,nBmpConEOT,
     O                C_EOTrvars,I_EOTrvars)

***** OPEN THE ANNUAL FILE AND SUM THE EOT LOADS FOR ALL RSEGS
      call AnnualEOTLoads(rscen,lenrscen,C_allup,nallup,
     I             sdate(1),edate(1),nBmpConEOT,
     I             I_DBG,
     O             AnnEOTLoads)

      if(I_DBG.ge.2) then
       print*,'... ... Aggregate annual EOT loads for ',rseg(:lenrseg)
       print*,'Year',(',',C_BMPconnameEOT(icon),icon=1,nBmpConEOT)
       do iyr = sdate(1),edate(1)
         print*,iyr,(AnnEOTLoads(iyr,icon),icon=1,nBmpConEOT)
       end do
      end if

***** GET THE LIST OF RIVER VARS AND DSNs
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)
      
      call getrflags(
     I               paramscen,lenparamscen,rseg,
     O               Nexits,idummy,idummy,idummy)
      print*,'Nexits = ',Nexits

      call WDMinfo(rscen,Nexits,nRvar,Rdsn,Rname)!POPULATE nRvar,Rdsn,Rname
      print*,'nRvar = ',nRvar

***** OPEN THE WDM
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)
      if (err.ne.0) go to 991

c      wdmfnam = outwdmdir//'river/'//rscen(:lenrscen)//
c     .            '/stream/'//rseg(:lenrseg)//'.wdm'
      wdmfnam = rseg(:lenrseg)//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)                           ! open main wdm read/write
      if (err .ne. 0) go to 991

***** IDENTIFY RVARS FOR EOT BMPCON and CALCULATE RIVER LOADS
      do iyr=sdate(1),edate(1)
        do ibcon=1,nBmpConEOT
          AnnRIVLoads(iyr,ibcon) = 0.0
        end do
      end do

      print*,'. ',nBmpConEOT
      do ibcon=1,nBmpConEOT
        print*,'.. ',I_EOTrvars(ibcon)
        do ieotrvar=1,I_EOTrvars(ibcon)
          do irvar=1,nRvar
            print*,C_EOTrvars(ibcon,ieotrvar),' = ',Rname(irvar),' ?'
            if(C_EOTrvars(ibcon,ieotrvar).eq.Rname(irvar)) then
              dsn = Rdsn(irvar)
              print*,'... adding ',C_BMPconnameEOT(ibcon),' ',
     .         Rname(irvar),dsn
              call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)
              ival = 1
              do iyr=sdate(1),edate(1)
               do imm=1,12
                do idd=1,ndaysinmonth(iyr,imm)
                 do ihr=1,24
                  AnnRIVLoads(iyr,ibcon) = AnnRIVLoads(iyr,ibcon) +
     .               hval(ival)
c                  print*,'xx ',iyr,imm,idd,ihr,hval(ival)
                  ival = ival + 1
                 end do
                end do
               end do
               print*,C_BMPconnameEOT(ibcon),Rname(irvar),iyr,
     .           AnnRIVLoads(iyr,ibcon)
              end do
            end if
          end do
        end do
      end do

***** CALCULATE ANNUAL LOAD REDUCTION FACTORS
      do ibcon=1,nBmpConEOT
        do iyr=sdate(1),edate(1)
          AnnEOTfactor(iyr,ibcon) =
     .        ( AnnRIVLoads(iyr,ibcon) - AnnEOTLoads(iyr,ibcon) )
          if ( AnnEOTfactor(iyr,ibcon).gt.0) then
            if ( AnnRIVLoads(iyr,ibcon) .gt. 0 ) then
              AnnEOTfactor(iyr,ibcon) = 
     .          AnnEOTfactor(iyr,ibcon)
     .          / AnnRIVLoads(iyr,ibcon)
            else
              AnnEOTfactor(iyr,ibcon) = 1.0
            end if
          else
            AnnEOTfactor(iyr,ibcon) = 0.0
          end if
          print*,ibcon,AnnEOTfactor(iyr,ibcon)
        end do
      end do

***** SUBTRACT OUT THE LOAD FROM THE RVAR USING ANNUAL FACTORS
***** & WRITE THE WDM
      do ibcon=1,nBmpConEOT
        do ieotrvar=1,I_EOTrvars(ibcon)
          do irvar=1,nRvar
            if(C_EOTrvars(ibcon,ieotrvar).eq.Rname(irvar)) then
              dsn = Rdsn(irvar)
              call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)
              ival = 1
              do iyr=sdate(1),edate(1)
               print*,'... eot redux ',C_BMPconnameEOT(ibcon),' ',
     .           Rname(irvar),dsn,AnnEOTfactor(iyr,ibcon)
               do imm=1,12
                do idd=1,ndaysinmonth(iyr,imm)
                 do ihr=1,24
                  hval(ival) = hval(ival) * AnnEOTfactor(iyr,ibcon)
                  ival = ival + 1
                 end do
                end do
               end do
              end do
              call puthourdsn(wdmfil,sdate,edate,dsn,nvals,hval)
            end if
          end do
        end do
      end do

***** CLOSE THE WDM
      call wdflcl(wdmfil,err)
      if (err.ne.0) go to 992

      print*,'... completed processing '//rseg(:lenrseg)

      stop

990   report(1) = 'Problem with opening file'
      report(2) = ' '
      report(3) = ' '
      go to 999

991   report(1) = 'Problem with opening wdm for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

992   report(1) = 'Problem closing wdm for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

997   report(1) = 'Error initializing sumout program'
      report(2) = 'mismatch of data types between program expectation'
      report(3) = ' and calling script'
      go to 999

998   report(1) = 'Error initializing land postprocessor'
      report(2) = 'not enough input data in calling script'
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end
