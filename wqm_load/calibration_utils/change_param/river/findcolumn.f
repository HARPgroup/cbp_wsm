************************************************************************
** finds the column that is occupied by the specified variable        **
************************************************************************
      subroutine findcolumn(tableline,varline,table,variable, 
     O           varcolumn,err)
      implicit none

      character*(*)table  ! table name
      character*(*) variable  ! variable name
      character*14 Ttab,Tvar

      integer varcolumn    ! column that contains the variable

      integer tablast,varlast,err,last

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
      end do
 
      return
992   err = 2
      return
993   err = 3
      return
      end
