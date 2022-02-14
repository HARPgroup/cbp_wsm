      subroutine avesums(allvalues,nallvalues,title)
      implicit none
      include 'sumsumstats.inc'

      real x(maxsites)  ! transfer vector to this variable

      real average,median
      external average,median

      real aves(nparms),meds(nparms),absaves(nparms),absmeds(nparms)
        !!! output variables
*************** END DECLARATIONS **************************************

      do np = 1,nparms  ! loop over all parameters

        nvalues = nallvalues(np)

        do nv = 1,nvalues
          values(nv) = allvalues(np,nv)
        end do

        aves(np) = average(values,nvalues,maxsites,err)
        meds(np) = median(values,nvalues,maxsites,err)

C        if (np.eq.1.or.np.eq.6.or.np.eq.7.or.
C     .      np.eq.8.or.np.eq.11.or.np.eq.12) then
          do nv = 1,nvalues
            x(nv) = (abs(values(nv)))
          end do
C        else
C          do nv = 1,nvalues
C            x(nv) = (abs(1.0-values(nv)))
C          end do
C        end if

        absaves(np) = average(x,nvalues,maxsites,err)
        absmeds(np) = median(x,nvalues,maxsites,err)

      end do

      title(15:20) = 'averag'
      print 1234,title,(aves(np),np=1,nparms)
      title(15:20) = 'median'
      print 1234,title,(meds(np),np=1,nparms)
      title(15:20) = 'absave'
      print 1234,title,(absaves(np),np=1,nparms)
      title(15:20) = 'absmed'
      print 1234,title,(absmeds(np),np=1,nparms)

      return

1234  format(a20,24(', ',f7.3))

      end
