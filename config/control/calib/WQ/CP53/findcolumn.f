************************************************************************
** finds the column that is occupied by the specified variable        **
************************************************************************
      subroutine findcolumn(
     I                      table,variable,fnam,
     M                      tableline,varline,
     O                      varcolumn,err)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'

      character*(*)table  ! table name
      character*(*) variable  ! variable name
      character*14 Ttab,Tvar

      integer varcolumn    ! column that contains the variable

      integer tablast,varlast

      character*(*) tableline,varline
  
      logical scompcase
      external scompcase
      
      err = 0

      if (tableline(2000-5:2000).ne.'      ') go to 993
      if (varline(2000-5:2000).ne.'      ') go to 993

      varcolumn = 1
      call findcomma(tableline,tablast)
      Ttab = tableline(:tablast-1)
      call trims(Ttab,last)
      call findcomma(varline,varlast)
      Tvar = varline(:varlast-1)
      call trims(Tvar,last)
      do while (.not.(scompcase(Ttab,table).and.
     .                scompcase(Tvar,variable)))
        if (tablast.eq.0.or.varlast.eq.0) go to 992
        varcolumn = varcolumn + 1
        call shift(tableline)
        call shift(varline)
        call findcomma(tableline,tablast)
        Ttab = tableline(:tablast-1)
        call trims(Ttab,last)
        call findcomma(varline,varlast)
        Tvar = varline(:varlast-1)
        call trims(Tvar,last)
C        print*,Ttab,' ',table,' ',Tvar,' ',variable
      end do
 
      return
********************* ERROR SPACE **************************************
992   report(1) = 'problem reading parameter file for river'
      report(2) = fnam
      report(3) = 'did not find table, variable '//table//variable
      go to 999

993   report(1) = 'problem reading parameter file for river'
      report(2) = fnam
      report(3) = 'file has line longer than 2000 characters'
      go to 999

999   call stopreport(report)

      end
