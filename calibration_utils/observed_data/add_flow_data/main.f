***************************************************************************
** Subroutine to read flow data from downloaded file  from USGS website  **
**                                                                       **
** Data Flag:                                                            **
** A = Approved                                                          **
*** P = Provisional data subject to revision                             **
** E = Estimated                                                         **
** Ae = Approved estimate data                                           **
** Pe = Provisional estimate data                                        **
***************************************************************************
      program main
      implicit none

      include '../../../src/lib/inc/standard.inc'
      include '../../../src/lib/inc/locations.inc'
      include '../../../src/lib/inc/rsegs.inc'

      integer nalldays
      parameter (nalldays = 3000) ! max number of days in simulation
      character*13 riverseg(maxrsegs)          ! reiver segment
      character*7  riverid(maxrsegs)           ! river segmeent ID

      integer fmcode,nrseg,nr

      character*110 dline
      character*100 exfnam,notefm,tfnam                    ! DATA file name
      character*7  catid, tmid                ! river segmeent ID   
  
      integer year, month, day,hour,zero
      integer i,j,ic,nd,ndays
      integer nsame,ndiff

      integer adyear(nalldays),admonth(nalldays),adday(nalldays)
      real adflow(nalldays),flow,diff,sumflow(nalldays)
      integer nov, ovyear(365),ovmonth(365),ovday(365)
      real  ovflow(365)

      logical found,newexist
************************ END DECLARATIONS ***********************************

      fnam = tree//'pp/observed/gage_riverseg_notes.csv'      ! get pairs of rsegs and ID
      open (unit=dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nrseg = 0
      read(dfile,'(a110)',err=992,end=111) dline                 ! get rid of the header 
      do
        read(dfile,'(a110)',err=992,end=111) dline               ! read line
***********loop over all river segments
        nrseg = nrseg + 1
        call findcomma(dline,ic)
        riverseg(nrseg)= dline(:ic-1)         ! get the name of river segment
        call shift(dline)
        call findcomma(dline,last)           ! get the segment ID
        riverid(nrseg) = dline(:last-1)
        if (dline(:last).ne.' ') then
         call shift(dline)
        end if
      end do                                 !loop over segments

111   close(dfile)
      
************open a note to record which river segments has big difference from old data          
      do nd = 1, nalldays
       sumflow(nd) = 0.0
      end do

      notefm=tree//'pp/observed/alldata/FLOW/NOTE.txt'
      open (unit=dfile+4,file=notefm,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      tfnam = tree//'pp/observed/alldata/FLOW/runinfo.txt'
      open (unit=dfile+10,file=tfnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      do nr = 1, nrseg
***********Open existing file to read data
       exfnam = tree//'pp/observed/alldata/FLOW_to_2002/'
     .             //riverseg(nr)//'.OFLOW'
       open (unit=dfile+2,file=exfnam,status='old',iostat=err)
       if (err.ne.0) cycle       ! when old data file exists
	
       call read_flow(riverseg(nr),riverid(nr),
     O                ndays,adyear,admonth,adday,adflow,newexist)

       print*, nr,' ', riverseg(nr), ' ',newexist
       write (dfile+10,2000) nr, riverseg(nr), newexist    ! diagnostic info

************** deal with specila case of JL7_6800_7070 ********************
      if (riverseg(nr) .eq. 'JL7_6800_7070' ) then     ! Seg JL7_6800_7070 has two USGS sites:
        do nd = 1, ndays                               ! ID: 2037000 - JAMES RIVER AND KANAWHA CANAL NEAR RICHMOND
         sumflow(nd) = sumflow(nd) + adflow(nd)        ! ID: 2037500 - MAIN STEM OF JAMES RIVER
         adflow(nd) = sumflow(nd)                      ! FLOW IS THE SUM OF FLOW FROM THESE TWO STATIONS
        end do
      end if

***********open NEW file to write data
       fnam = tree//'pp/observed/alldata/FLOW/'//riverseg(nr)//'.OFLOW'
       open (unit=dfile+3,file=fnam,status='unknown',iostat=err)
       if (err.ne.0) go to 991

       print*, 'start to write data for river segment ',riverseg(nr)
 
       nov = 0

       if (.not.newexist) then               ! If old data exist but no new data
        do                                   
         read(dfile+2,*,err=992,end=222) year,month,day,hour,zero,flow
         write(dfile+3,1001) year,month,day,hour,zero,flow      ! write all old data 
        end do 
222   close(unit=dfile+2)

       else                                  ! if new data exist
        do 
         read(dfile+2,*,err=992,end=333) year,month,day,hour,zero,flow
         if (year .lt. adyear(1)) then
          write(dfile+3,1001) year,month,day,hour,zero,flow      ! before overlap, write data from old file
         else                                                    ! find overlaped date and flow
          nov = nov + 1 
          ovyear(nov)  = year
          ovmonth(nov) = month
          ovday(nov)   = day
          ovflow(nov)  = flow
         end if
        end do 
333    close(unit=dfile+2) 
************ compare overlaped data, addnew data to the file
        nsame = 0
        ndiff = 0
        do i = 1, ndays
         do j = 1,nov
          if(adyear(i).eq.ovyear(j) .and. admonth(i).eq.ovmonth(j) 
     .       .and. adday(i).eq.ovday(j)) then
           nsame = nsame + 1                                   
           diff = (adflow(i)-ovflow(j))/ovflow(j)               ! calculate the difference between old and new file
           if (abs(diff) .gt. 0.1) ndiff = ndiff + 1         
          end if
         end do
        write(dfile+3,1001) adyear(i),admonth(i),adday(i),12,0,  ! for the overlaped record, use new data
     .                      adflow(i)
        end do                                !loop over records for a segment

       close(dfile+3)   
  
        if (real(ndiff)/real(nsame) .gt. 0.1) then
        write(dfile+4, 1002) riverseg(nr), 'has more than 10% of values 
     .  greater than 10% different from old data'
        end if  
       
       end if

      end do                                 !loop over segments
      
      close(dfile+10)
      close(dfile+4)

1001   format(i4,',',i2,',',i2,',',i2,',',i2,',',e12.5)
1002   format(a13,' ',a70)
2000   format(i3,', ',a13,', ',l1)  
      stop 

************************ ERROR SPACE
991   report(1) = 'Problem opening output file'//fnam
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

992   report(1) = 'Problem opening data file'//exfnam
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end 
