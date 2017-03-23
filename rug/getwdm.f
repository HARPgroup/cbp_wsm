************************************************************************
**  get the name of a wdm associated with a land segment              **
**    Inputs: lseg,  land or river segment                            **
**            Stype, Vtype:  Segment type and Variable type           **
**    Outputs: fnam, found: file name, logical found/notfound         **
**    gets files from catdir//data/land_$type_wdm.csv                 **
************************************************************************
      subroutine getwdm(seg,Stype,Vtype,rscen,lenrscen,
     O                  fnam,found)
      implicit none

      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'

      character*5 Stype         ! either 'land' or 'river'
      character*5 Vtype         ! either 'met' or 'prad'
      integer lenStype,lenVtype
      logical scompare,scompcase,found
      character*(*) seg

      integer ic

      character*64 wdmnamefile

      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      call lencl(Vtype,lenVtype)
      call lencl(Stype,lenStype)
      wdmnamefile = catdir//'geo/'//geoscen(:lengeoscen)//'/'//
     .              Stype(:lenStype)//'_'//
     .              Vtype(:lenVtype)//'_wdm.csv'
      open(dfile,file=wdmnamefile,status='old',iostat=err)     ! open 
      if (err.ne.0) go to 991

      found = .false.
      read(dfile,'(a100)') line
      call d2x(line,last)
      call findcomma(line,ic)
      if (.not.(scompcase(line(:ic-1),Stype)))  go to 992
      do while (line(:3).ne.'end')
        read(dfile,'(a100)')line
        call d2x(line,last)
        call findcomma(line,ic)
        if (scompare(line(:ic-1),seg)) then
          call shift(line)
          call findcomma(line,last)
          fnam = line(:last)
          found = .true.
          go to 888
        end if
      end do

888   close (dfile)

      if (.not.found) go to 993

      return

************** ERROR SPACE *********************************************
991   report(1) = 'Could not open file'
      report(2) = wdmnamefile
      report(3) = 'error = '
      write(report(3)(9:10),'(i2)') err
      go to 999

992   report(1) = 'First line in file:  should be header, e.g.'
      report(2) = wdmnamefile
      report(3) = '[LAND or River], wdm'
      go to 999

993   report(1) = 'did not find segment '//seg
      report(2) = 'in file'
      report(3) = wdmnamefile
      go to 999

999   call stopreport(report)

      end

************************************************************************
**  get the name of a wdm associated with a land segment              **
**    Inputs: rseg, lseg : land and river segments                    **
**            Vtype:   Variable type                                  **
**    Outputs: fnam, found: file name, logical found/notfound         **
**    gets files from catdir//data/land_$type_wdm.csv                 **
************************************************************************
      subroutine getlrwdm(rseg,lseg,Vtype,
     O                    fnam,found)
      implicit none

      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'

      character*5 Vtype         ! probably 'pssep'
      integer lenVtype

      character*64 wdmnamefile

      integer ic

      character*13 Tseg1,Tseg2
      logical scompcase,found
      logical landfirst  
          ! landfirst means that the land column comes before the river


      call lencl(Vtype,lenVtype)
      wdmnamefile = catdir//'data/'//Vtype(:lenVtype)//'_wdm.csv'
      open(dfile,file=wdmnamefile,status='old',iostat=err)     ! open
      if (err.ne.0) go to 991

      read(dfile,'(a100)') line                    ! read the header line
      call columnorder(wdmnamefile,line,landfirst) ! determine first column

      found = .false.                         ! search for the rseg-lseg pair
      if (landfirst) then
        Tseg1 = lseg
        Tseg2 = rseg
      else
        Tseg1 = rseg
        Tseg2 = lseg
      end if
      do while (line(:3).ne.'end')
        read(dfile,'(a100)')line
        call d2x(line,last)
        call findcomma(line,ic)
        if (scompcase(line(:ic-1),Tseg1)) then
          call shift(line)
          call findcomma(line,ic)
          if (scompcase(line(:ic-1),Tseg2)) then
            call shift(line)
            call findcomma(line,last)
            fnam = line(:last)
            found = .true.
            go to 888
          end if
        end if
      end do

888   close (dfile)

      return

************** ERROR SPACE *********************************************
991   report(1) = 'Could not open file'
      report(2) = wdmnamefile
      report(3) = 'error = '
      write(report(3)(9:10),'(i2)') err
      go to 999

999   call stopreport(report)

      end

