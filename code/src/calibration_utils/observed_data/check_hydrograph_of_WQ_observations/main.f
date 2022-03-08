************************************************************************
**  program to read observed files and compare the distribution of    **
**   flows on the days with observed data overall flow distribution   **
************************************************************************
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      character*4 const  ! WQ constituent

      real ObFlowDate(1900:2100,12,31)
      real ObFlow(20000)
      real PairedFlow(2000)

      real ObHazen(20000)
      real PairedHazen(2000)

      integer ordOF(20000)  ! sorted order
      integer ordPF(2000)   ! sorted order

      real value

      integer year,month,day,hour,minute,year1,year2

      integer nObFlowDays,nPairedFlowDays

      integer nO,nP

      real KsKstat

      logical found

      real PointHazen
      
************** END DECLARATIONS ****************************************
      read*,rseg,const,year1,year2

********** initialize
      do year = 1900,2100
        do month = 1,12
          do day = 1,31
            ObFlowDate(year,month,day) = -9.0
          end do
        end do
      end do

********** open files
      fnam = calibdir//'observed/alldata/FLOW/'//rseg//'.OFLOW'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.eq.2) stop

      fnam=calibdir//'observed/alldata/'//const//'/'//rseg//'.O'//const
      open(dfile+1,file=fnam,status='old',iostat=err)
      if (err.eq.2) stop

      write(*,'(a13,a1,a4,a1,$)') rseg,',',const,','
************ read flow file
      nObFlowDays = 0
      do
        read(dfile,'(a100)',end=111) line
        call d2x(line,last)
        read(line,*) year,month,day,hour,minute,value
        if (year.ge.year1.and.year.le.year2) then
          nObFlowDays = nObFlowDays + 1
          ObFlowDate(year,month,day) = value
          ObFlow(nObFlowDays) = value
        end if
      end do

111   close(dfile)

      if (nObFlowDays.eq.0) then
        write(*,*) 'No Observed Flow in years'
        stop
      end if

********** read data file and get paired flows
      nPairedFlowDays = 0
      do
        read(dfile+1,'(a100)',end=222) line
        call d2x(line,last)
        read(line,*) year,month,day,hour,minute,value
        if (year.ge.year1.and.year.le.year2) then
          if (ObFlowDate(year,month,day).gt.-8.0) then
            nPairedFlowDays = nPairedFlowDays + 1
            PairedFlow(nPairedFlowDays) = ObFlowDate(year,month,day)
          end if
        end if
      end do

222   close(dfile)

      if (nPairedFlowDays.eq.0) then
        write(*,*) 'No Paired Observed Flow '
        stop
      end if

      if (nPairedFlowDays.lt.10) then
        write(*,*) 'not enough paired days'
        stop
      end if

***************** take logs
      do nO = 1,nObFlowDays
        ObFlow(nO) = log10(ObFlow(nO))
        ObHazen(nO) = (real(nO) - 0.5)/ real(nObFlowDays)
      end do
      do nP = 1,nPairedFlowDays
        PairedFlow(nP) = log10(PairedFlow(nP))
        PairedHazen(nP) = (real(nP) - 0.5)/ real(nPairedFlowDays)
      end do

*********** sort
      call qsortr(ordOF,nObFlowDays,ObFlow)
      call qsortr(ordPF,nPairedFlowDays,PairedFlow)

************************ calculate KS statistic
************ do double pass and calculate the probability axis 
***************** distance between the point and line
      KsKstat = -1.0

********* loop over the observed flow first.
******** when two paired points straddle it on the log flow axis, calc
*********** the distance between the observed point and the line
********** connecting the two paired points.
*********** test is one-tailed for the paired data being lower than the full
*********** data set
      do nO = 1,nObFlowDays
        found = .false.
        do nP = 2,nPairedFlowDays
          if (PairedFlow(ordPF(nP)).ge.ObFlow(ordOF(nO)).and.
     .        PairedFlow(ordPF(nP-1)).le.ObFlow(ordOF(nO))) then
            found = .true.
            exit
          end if
        end do
        if (found) then
          PointHazen = PairedHazen(nP-1) + 
     .                (PairedHazen(nP)-PairedHazen(nP-1)) * 
     .                (ObFlow(ordOF(nO)) - PairedFlow(ordPF(nP-1))) / 
     .                (PairedFlow(ordPF(nP)) - PairedFlow(ordPF(nP-1)))
          KsKstat = max(KsKstat,(PointHazen-ObHazen(nO)))
        end if
      end do

********** then loop over paired, doing the same thing
      do nP = 1,nPairedFlowDays
        found = .false.
        do nO = 2,nObFlowDays
          if (ObFlow(ordOF(nO)).ge.PairedFlow(ordPF(nP)).and.
     .        ObFlow(ordOF(nO-1)).le.PairedFlow(ordPF(nP))) then
            found = .true.
            exit
          end if
        end do
        if (found) then
          PointHazen = ObHazen(nO-1) + 
     .                (ObHazen(nO)-ObHazen(nO-1)) * 
     .                (PairedFlow(ordPF(nP)) - ObFlow(ordOF(nO-1))) / 
     .                (ObFlow(ordOF(nO)) - ObFlow(ordOF(nO-1)))
          KsKstat = max(KsKstat,(PairedHazen(nP)-PointHazen))
        end if
      end do

      if (ksKstat.lt.-.9) ksKstat = 1.0  ! no overlap at all

      write(*,*) ksKstat,',', nPairedFlowDays

      stop
      end
