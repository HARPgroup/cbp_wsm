************************************************************************
** set of variables that are frequently used in programs that convert **
**  the unique ID into the 13 character segment name or determine     **
**   watershed size, or otherwise read in all river segments          **
************************************************************************
      integer maxrsegs,supermax,nrsegs
      parameter (maxrsegs=1500,supermax=9999)

      integer uniqid(maxrsegs)           ! 4 digit ID
      integer dsid(maxrsegs)             ! 4 digit ID for downstream segments
      character*13 rsegs(maxrsegs)       ! 13 character ID
      integer uniqindex(supermax)        ! returns segments index for uniqid

