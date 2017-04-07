      implicit none
      integer nplu,nilu,nptab,nitab
      parameter (nplu=3,nilu=1,nptab=3,nitab=3)
      character*3 plu(nplu),ilu(nilu)
      character*10 ptab(nptab),itab(nitab)

      data plu /'crp','for','pur'/
      data ilu /'imp'/
      data ptab /'ATEMP.csv','PWATER.csv','SNOW.csv'/
      data itab /'ATEMP.csv','IWATER.csv','SNOW.csv'/

      integer nl,nt,i,last

      character*3000 line(400),saveline
      character*100 fnam

      character*6 segfrom,segto,seg,nextseg
      integer isegto,iseg,inextseg

      logical found

      read *,segfrom,segto,fnam
      print*,segfrom,segto, fnam
1234  format(a,a,a)

      open(11,file=fnam,status='old')
      nl = 0
      do while (line(nl)(:3).ne.'end')
        nl = nl + 1
        read(11,'(a3000)')line(nl)
      end do
      close(11)

      open(11,file=fnam,status='old')
      do i = 1,nl  ! find the place where this one goes
        call ryt(line(i),11)
        if (line(i)(:6).eq.segfrom) then
          line(i)(:6) = segto
          call ryt(line(i),11)
        end if
      end do
      end
************************************************************************
**  subroutine ryt writes a line to the specified file with no blanks **
************************************************************************
      subroutine ryt(line,ifn)
      implicit none
      character*(*),line
      integer ifn,last,lenline,i

      lenline = len(line)
      last = -1
      do i=1,lenline
        if (line(i:i).ne.' ') last = i
      end do

      if (last.gt.0) then
        write(ifn,'(a)') line(:last)
      else
        write(ifn,*)
      end if

      end


         

