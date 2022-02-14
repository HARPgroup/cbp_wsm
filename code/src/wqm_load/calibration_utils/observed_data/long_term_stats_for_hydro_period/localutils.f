************************************************************************
** subroutine to turn a maxtrix with specific format into a vector    **
**  vector will only contain years specified in 'doyear'              **
************************************************************************
      subroutine vectorize (
     I                      xy,doyear,
     I                      firstyear,lastyear,maxvals,
     O                      x,nvals)
      implicit none
      integer firstyear,lastyear,maxvals,nvals
      real xy(firstyear:lastyear,366)
      logical doyear(firstyear:lastyear)
      real x(maxvals)

      integer ny,nd
      integer ndaysinyear 
      external ndaysinyear

      nvals = 0
      do ny = firstyear,lastyear
        if (doyear(ny)) then
          do nd = 1,ndaysinyear(ny)
            nvals = nvals + 1
            x(nvals) = xy(ny,nd)
          end do
        end if
      end do
      return
      end

************************************************************************
** subroutine to turn a maxtrix with specific format into a vector    **
**  vector will only contain years specified in 'doyear'              **
**  gives total annual flow, rather than daily                        **
************************************************************************
      subroutine annvector (
     I                      xy,doyear,
     I                      firstyear,lastyear,maxyears,
     O                      annx,nyears)
      implicit none
      integer firstyear,lastyear,maxyears,nyears
      real xy(firstyear:lastyear,366)
      logical doyear(firstyear:lastyear)
      real annx(maxyears)

      integer ny,nd
      integer ndaysinyear 
      external ndaysinyear

      nyears = 0
      do ny = firstyear,lastyear
        if (doyear(ny)) then
          nyears = nyears + 1
          annx(nyears) = 0.0
          do nd = 1,ndaysinyear(ny)
            annx(nyears) = annx(nyears) + xy(ny,nd)
          end do
        end if
      end do
      return
      end

************************************************************************
** subroutine to open, write, and close a matlab data file            **
************************************************************************
      subroutine writematlab(fnam,x,n,maxn)
      implicit none
      character*(*) fnam
      integer n,maxn,i,err
      real x(maxn)
      character*64 report(3)

      open(13,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) then
        report(1) = 'Problem opening data file, '//fnam
        report(2) = '  Error =    '
        write(report(2)(11:13),'(i3)') err
        report(3) = ' '
        call stopreport(report)
      end if
      do i = 1,n
        write(13,*)x(i)
      end do
      close(13)
      return
      end
