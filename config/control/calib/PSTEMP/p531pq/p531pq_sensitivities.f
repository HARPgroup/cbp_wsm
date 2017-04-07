************************************************************************
** The parameters calibrated for temperature are the values of the    **
**   intercepts on the first day of the month.  The actual value used **
**   for any given day depends on the value for the month and the     **
**   value for next month.  Therefore, the temperature calculation in **
**   the month of the parameter and the previous month are both       **
**   equally sensitive to the parameter value (Feb and March temps    **
**   are sensitive to the March 1 value of the parameter)             **
** These functions take the temperature biases of the two surronding  **
**   months to do the update calculation                              **
** As the aveage bias goes to zero, the factors should go to zero     **
************************************************************************
      function calfacASLT(lastSFTbias,SFTbias,lastBFTbias,BFTbias)
      implicit none
      real calfacASLT,lastSFTbias,SFTbias,lastBFTbias,BFTbias,ave
      ave = ( SFTbias + lastSFTbias ) / 2.0
      calfacASLT = - ave / 2.0
      end

      function calfacULTP1(lastSFTbias,SFTbias,lastBFTbias,BFTbias)
      implicit none
      real calfacULTP1,lastSFTbias,SFTbias,lastBFTbias,BFTbias,ave
      ave = ( SFTbias + lastSFTbias ) / 2.0
      calfacULTP1 = - ave / 2.0
      end

      function calfacLGTP1(lastSFTbias,SFTbias,lastBFTbias,BFTbias)
      implicit none
      real calfacLGTP1,lastSFTbias,SFTbias,lastBFTbias,BFTbias,ave
      ave = ( BFTbias + lastBFTbias ) / 2.0
      calfacLGTP1 = - ave / 1.5
      end

