************************************************************************
** function returns 2-digit state fips number from 2-character state  **
************************************************************************
      function fips2state(fn)
      implicit none
      character*2 fips2state
      integer fn
      if (fn.eq.51) then
        fips2state = 'VA'
        return
      else if (fn.eq.42) then
        fips2state = 'PA'
        return
      else if (fn.eq.24) then
        fips2state = 'MD'
        return
      else if (fn.eq.54) then
        fips2state = 'WV'
        return
      else if (fn.eq.36) then
        fips2state = 'NY'
        return
      else if (fn.eq.10) then
        fips2state = 'DC'
        return
      else if (fn.eq.11) then
        fips2state = 'DE'
        return
      else if (fn.eq.37) then
        fips2state = 'NC'
        return
      else if (fn.eq.47) then
        fips2state = 'TN'
        return
      else
        fips2state = 'XX'
        return
      end if
      end


************************************************************************
** function returns 2-character state from 2-digit state fips number  **
************************************************************************
      function state2fips(st)
      implicit none
      character*2 st
      integer state2fips
      if (st.eq.'VA') then
        state2fips = 51
        return
      else if (st.eq.'PA') then
        state2fips = 42
        return
      else if (st.eq.'MD') then
        state2fips = 24
        return
      else if (st.eq.'WV') then
        state2fips = 54
        return
      else if (st.eq.'NY') then
        state2fips = 36
        return
      else if (st.eq.'DC') then
        state2fips = 10
        return
      else if (st.eq.'DE') then
        state2fips = 11
        return
      else if (st.eq.'NC') then
        state2fips = 37
        return
      else if (st.eq.'TN') then
        state2fips = 47
        return
      else
        state2fips = 00
        return
      end if
      end
