First you need :
        /output/input/$scen/*monthly*
        /output/eor/monthly/$scen
           use: /bluefish/archive/modeling/gb560/run_bhatt/OneCommandWSM_P6_GY/gy_etm_monthly_parallel.csh

1. Use run_input_monthly_load_parallel.csh to create lrseg monthly input loads (lbs) for all landuses (ag, urban and feedspace) and for all input types (atdep, manure, fert, legume , and uptake). 

2. Use run_summary_input_load_lu_parallel.csh to summarize basin loads for all landuses (ag, urban and feedspace) and input types.

3. Use run_summary_input_load_basin_and_ps.csh to summarize basin loads for all input types  and point sources.

e.g. run_summary_input_load_basin_and_ps.csh P620160401WQa_STRLOAD watershed 1984 2014 
