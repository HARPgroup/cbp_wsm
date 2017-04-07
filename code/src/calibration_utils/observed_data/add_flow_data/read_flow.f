***************************************************************************
** Subroutine to read flow data downloaded from USGS website             **
**                                                                       **
** Data Flag:                                                            **
** A = Approved                                                          **
** P = Provisional data subject to revision                              **
** E = Estimated                                                         **
** Ae = Approved estimate data                                           **
** Pe = Provisional estimate data
** Blank = No flag                                                       **
************************************************************************
       subroutine read_flow(thisrseg,segid,
     O                      nd,adyear,admonth,adday,adflow,newexist)
       implicit none

       include '../../../src/lib/inc/standard.inc'
       include '../../../src/lib/inc/locations.inc'
       include '../../../src/lib/inc/rsegs.inc'

       integer nalldays
       parameter (nalldays = 3000) ! max number of days in simulation

       character*7  segid,tmid               ! river segmeent ID
       character*200 dline
       character*100 newfnam      
       integer fmcode,ncomma,nc

       character*4  usgs
       character*8  sid
       character*10 cdate
       character*2  flag
       character*14 headline
       character*13 thisrseg
   
       integer adyear(nalldays),admonth(nalldays),adday(nalldays)
       integer ic,nd,nstn,i
       real adflow(nalldays),mvalue
       real flow(nalldays),sumflow(nalldays)

       logical:: found,newexist
************************* END DECLARATIONS ***********************************
       newfnam = tree//'pp/observed/CBwatershed_flow_data.csv'
       open (unit=dfile+1,file=newfnam,status='old',iostat=err)
       if (err.ne.0) go to 991
            
        found=.false.
        newexist = .false.
        do while (.not. found)
         read(dfile+1,'(a200)',err=992,end=993) dline
         call findcomma(dline,ic)
         usgs = dline(:ic-1) 
         call trims(usgs,last)

************loop to find the segment
         found=.false.
         if (usgs(:last) .eq. 'USGS') then
          call shift(dline)
          call findcomma(dline,ic)
          tmid = dline(2:ic-1)
          call trims(tmid,last)
         
          if (tmid(:last) .eq. segid) then
           found = .true.
           backspace(dfile+1)                 ! back to the first header line of found segment
           backspace(dfile+1)
           backspace(dfile+1)
           read(dfile+1,'(a200)',err=992,end=993) dline
           call findcomma(dline,ic)
           headline = dline(:ic-1)
           call trims(headline,last)
           ncomma = 1

           do while (headline(4:last) .ne. '00060_00003')    ! find the column where flow data resides
            ncomma = ncomma + 1
            call shift(dline) 
            call findcomma(dline,ic)
            headline = dline(:ic-1)
            call trims(headline,last)
           end do
           read(dfile+1,'(a200)',err=992,end=993) dline       ! get rid of second header line
          end if
         end if
        end do

*********** read the record for the segment
        nd = 0
        nstn = 0
        do while (found)
         read(dfile+1,'(a200)',err=992,end=993) dline
         call shift(dline)
         call findcomma(dline,ic)
         tmid = dline(2:ic-1)
         call trims(tmid,last)

         if (tmid .eq. segid) then
          call shift(dline)
          call findcomma(dline,ic)
          cdate = dline(:ic-1)
          if (ncomma .eq. 4) then           ! only mean flow data
           call shift(dline)
           call findcomma(dline,ic)
          else if (ncomma .eq. 8) then      ! include max, min and mean flow
           do nc = 1, 5
            call shift(dline)
            call findcomma(dline,ic)
           end do
          end if

          if (dline(:ic-1).eq. ' ' .or. dline(:ic-1).eq.'Ice'.or.    ! Ice=Ice affected; Ssn=Parameter monitored seasonally  
     .        dline(:ic-1).eq.'Ssn'.or. dline(:ic-1).eq.'Eqp'.or.    ! Eqp = Equipment malfunction
     .        dline(:ic-1).eq.'Rat' .or.dline(:ic-1).eq.'Fld'.or.    ! Rat = Rating being developed or revised 
     .        dline(:ic-1).eq.'***') then                            ! Fld = Flood damage
           print*, 'no data on ',cdate,', skip this day'              
          else
           read(dline,*,err=992,end=993) mvalue,flag
           if (flag.eq.'A' .or. flag.eq.'Ae' .or. flag.eq.'P'
     .          .or. flag.eq.'Pe') then                        ! data with these three flags are fine
            nd = nd + 1
            read(cdate(1:4), '(i4)') adyear(nd)
            read(cdate(6:7),'(i2)') admonth(nd)
            read(cdate(9:10),'(i2)') adday(nd)
            adflow(nd) = mvalue
            newexist =.true.      
           end if
          end if    
         else     
          found = .false.
         end if      
        end do   
     
993   close(dfile+1)
                    
      return 

************************ ERROR SPACE **************************************
991   report(1) = 'Problem opening data file'//newfnam
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading data from opened data file '
      report(2) = newfnam
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end 
