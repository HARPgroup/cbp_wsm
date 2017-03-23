************************************************************************
** looks for missing values (less than -998) and populates them with  **
**  neighbors.  reports the number of missing values found            **
************************************************************************
      subroutine fixmiss(dval,nvals,
     O                   missing)
      implicit none
      integer nvals,missing
      real dval(nvals)

      integer n,n2,n3,firstreal,lastreal,prevreal,nextreal

      missing = 0
      firstreal = 1
      lastreal = nvals

      do while (dval(firstreal).lt.-998.0)
        firstreal = firstreal + 1
        if (firstreal.eq.nvals) then
          missing = nvals
          return
        end if
      end do

      do while (dval(lastreal).lt.-998.0)
        lastreal = lastreal - 1
      end do

      do n = 1,firstreal-1  ! fill in initial missing
        dval(n) = dval(firstreal)
        missing = missing + 1
      end do

      do n = nvals,lastreal+1,-1  ! fill in trailing missing
        dval(n) = dval(lastreal)
        missing = missing + 1
      end do

      n = firstreal 
      do     ! fill in rest
        n = n + 1
        if (n.eq.lastreal) exit
        if (dval(n).le.-998.0) then
          prevreal = n - 1
          do while (dval(n).le.-998.0)
            n = n + 1
          end do
          nextreal = n
          do n = prevreal+1,nextreal-1
            missing = missing + 1
            dval(n) = dval(prevreal)+ ((dval(nextreal)-dval(prevreal))
     .                      * real(n-prevreal)/real(nextreal-prevreal))
          end do
          n = nextreal
        end if
      end do
      return
      end
         

