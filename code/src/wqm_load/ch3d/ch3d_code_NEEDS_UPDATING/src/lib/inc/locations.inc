************************************************************************
***** PARAMETER FILE  -- changing this file changes the parameters in **
***** the entire pre- and post-processors ( recompile necessary )     **
************************************************************************

********* parameters giving the locations of input and output files
      character*12 tree        ! location of hspf files
      parameter (tree   = '/model/p518/')

      character*23 catdir        ! location of catalog files
      parameter (catdir = tree//'pp/catalog/')

      character*21 pardir        ! location of parameter files
      parameter (pardir = tree//'pp/param/')

