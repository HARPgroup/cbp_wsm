************************************************************************
***** land constituents with names that match wdm variables and       **
*****    surrogates that match BMP variables                          **
************************************************************************

      integer nbcon,kbflo,kbsed,kbtn,kbtp,kbbod
********* parameters to size the problem
      parameter (nbcon=5)           ! number of constituents
 
******** the following parameters correspond to nbcon
      character*3 bconname(nbcon)

      data bconname /'flo','sed','tn ','tp ','bod'/  ! must match up with the
                                                     ! surrogates in land_const


      
