      subroutine getmetreg(
     I                     tree,lseg,
     O                     metregion)
      implicit none
      character*(*) tree
      integer metregion,ifl
      character*6 lseg,tlseg
      logical scompcase
      external scompcase
      character*150 fnam

      call findopen(ifl)
      fnam = tree//'input/unformatted/p4met/p5_met_stations.csv'
      open (ifl,file=fnam,status='old')

      do 
        read(ifl,*) tlseg,metregion
        if (scompcase(tlseg,lseg)) return
      end do
      end

      

