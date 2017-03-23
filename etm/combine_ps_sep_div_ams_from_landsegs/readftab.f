************************************************************************
**  read water surface from the standard ftable                       **
************************************************************************
      subroutine read_Sftable(SFarea)

      implicit none
      include 'combine_ps_sep_div_from_landsegs.inc'
      integer nrows             ! number of rows
      real SFarea

      read(dfile,'(a)')line
      nrows = 1 
      do while (line(:12).ne.'  END FTABLE')
        read(dfile,'(a)')line
        call d2x(line,last)
        nrows = nrows + 1
        if (nrows .eq. 10) then          ! if the fifth row of ftable 
        read(line(11:20),'(f10.3)') SFarea   ! read surface area    
        exit
        end if 
      end do

      close(dfile)

      return
      
      end
************************************************************************
**   read water surface from the variable ftable                      **
************************************************************************
      subroutine read_Vftable(fnam, SFarea)

      implicit none
      include 'combine_ps_sep_div_from_landsegs.inc'

      integer nrows             ! number of rows 
      real SFarea
********** END DECLARATION *********************************************

      read(dfile,'(a)') line
      do while (line(:8).ne.'  FTABLE')
        read(dfile,'(a)') line
      end do

      nrows = 1
      do while (line(:12).ne.'  END FTABLE')
        read(dfile,'(a)')line
        call d2x(line,last)
        nrows = nrows + 1
        if (nrows .eq. 10) then          ! if the fifth row of ftable
        read(line(11:20),'(f10.3)') SFarea   ! read surface area
        exit
        end if
      end do

      close(dfile)

      return

      end
