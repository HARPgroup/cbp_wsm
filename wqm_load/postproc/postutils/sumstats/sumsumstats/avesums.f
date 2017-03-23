      subroutine avesums(values,nvalues,title)
      implicit none
      include 'sumsumstats.inc'

      real x(maxsites)  ! transfer vector to this variable

      real average,median
      external average,median

      real aves(nparms),meds(nparms),absaves(nparms),absmeds(nparms)
        !!! output variables
*************** END DECLARATIONS **************************************

      do np = 1,nparms  ! loop over all parameters

        do nv = 1,nvalues
          x(nv) = values(np,nv)
        end do

        aves(np) = average(x,nvalues,maxsites,err)
        meds(np) = median(x,nvalues,maxsites,err)

        if (np.eq.1.or.np.eq.6.or.np.eq.7.or.
     .      np.eq.8.or.np.eq.11.or.np.eq.12) then
          do nv = 1,nvalues
            x(nv) = (abs(values(np,nv)))
          end do
        else
          do nv = 1,nvalues
            x(nv) = (abs(1.0-values(np,nv)))
          end do
        end if

        absaves(np) = average(x,nvalues,maxsites,err)
        absmeds(np) = median(x,nvalues,maxsites,err)

      end do

      title(8:13) = 'averag'
      print 1234,title,(aves(np),np=1,nparms)
      title(8:13) = 'median'
      print 1234,title,(meds(np),np=1,nparms)
      title(8:13) = 'absave'
      print 1234,title,(absaves(np),np=1,nparms)
      title(8:13) = 'absmed'
      print 1234,title,(absmeds(np),np=1,nparms)

      return

1234  format(a13,14(', ',f7.3))

      end
