% This program calculates the 2-vector, 2-tailed KS test to look for 
% statistical differences in the regional factors in the coastal plain
%
%  The numbers are calculated from the calibration spreadsheet '$scen ungaged basins.xls'
% 
% This version is for the p531 calibrations
%
% ES = eastern shore
% UW = upper western shore
% LW = lower western shore
%
% CPD = coastal plain disected upland
% CPU = coastal plain upland
% PCR = Piedmont Crystalline
%
% A = AGCHEM calibration
% P = PQUAL calibration
%


ES_A_tn = [ 0.707, 0.500, 0.500 ]

UW_A_tn = [ 0.889, 0.904, 1.633, 0.500, 0.948, 0.500 ]

LW_A_tn = [ 0.666, 0.500, 0.569, 0.500 ]

ES_A_tp = [ 0.500, 0.500, 0.500 ]

UW_A_tp = [ 0.794, 1.179, 0.575, 2, 1.189, 0.689, 0.737, 1.905]

LW_A_tp = [ 0.695, 0.822, 1.401, 0.728, 0.81, 0.631, 0.5]

%CPD_A_tn = [ 0.87,0.5,0.5]

CPU_A_tn = [ 0.5, 0.707, 0.5, 0.5, 0.5, 0.5, 0.569, 0.5]

PCR_A_tn = [ 1.221, 0.666, 0.948, 0.889, 0.904, 1.663 ]

%CPD_A_tp = [ 1.033,2,1.238]

CPU_A_tp = [ 0.737, 2, 0.5, 0.5, 0.5, 0.5, 1.401, 0.81, 0.689 ]

PCR_A_tp = [ 0.631, 1.905, 0.695, 0.822, 0.728, 1.189, 0.794, 1.179, 0.575 ]

%ES_P_tn = [ 0.909,0.727,0.5]

%UW_P_tn = [ 0.895,0.997,1.683,0.5,0.933,0.5,1.159]

%LW_P_tn = [ 1,0.919,0.5,1,0.746,1,0.5]

%ES_P_tp = [ 0.544,0.65,0.5]

%UW_P_tp = [ 0.679,0.815,0.723,2,1.134,0.954,1.113,2]

%LW_P_tp = [ 0.928,0.979,1.559,0.933,1.33,0.975,0.523]

%CPD_P_tn = [ 0.997,0.5,0.5,1.159,1.159]

%CPU_P_tn = [ 0.909,0.727,0.5,0.5,0.746,0.5]

%PCR_P_tn = [ 0.895,0.997,1.683,0.933,1.159,0.919,0.5]

%CPD_P_tp = [ 0.815,2,0.954,2,2]

%CPU_P_tp = [ 0.544,0.65,0.5,1.559,1.33,0.523]

%PCR_P_tp = [ 0.679,0.815,0.723,1.134,1.113,2,0.928,0.979,0.933,0.975,0.523]

[H,P] = kstest2(ES_A_tn,UW_A_tn)
[H,P] = kstest2(UW_A_tn,LW_A_tn)
[H,P] = kstest2(LW_A_tn,ES_A_tn)

[H,P] = kstest2(ES_A_tp,UW_A_tp)
[H,P] = kstest2(UW_A_tp,LW_A_tp)
[H,P] = kstest2(LW_A_tp,ES_A_tp)

%[H,P] = kstest2(CPD_A_tn,CPU_A_tn)
[H,P] = kstest2(CPU_A_tn,PCR_A_tn)
%[H,P] = kstest2(PCR_A_tn,CPD_A_tn)

%[H,P] = kstest2(CPD_A_tp,CPU_A_tp)
[H,P] = kstest2(CPU_A_tp,PCR_A_tp)
%[H,P] = kstest2(PCR_A_tp,CPD_A_tp)

%[H,P] = kstest2(ES_P_tn,UW_P_tn)
%[H,P] = kstest2(UW_P_tn,LW_P_tn)
%[H,P] = kstest2(LW_P_tn,ES_P_tn)

%[H,P] = kstest2(ES_P_tp,UW_P_tp)
%[H,P] = kstest2(UW_P_tp,LW_P_tp)
%[H,P] = kstest2(LW_P_tp,ES_P_tp)

%[H,P] = kstest2(CPD_P_tn,CPU_P_tn)
%[H,P] = kstest2(CPU_P_tn,PCR_P_tn)
%[H,P] = kstest2(PCR_P_tn,CPD_P_tn)

%[H,P] = kstest2(CPD_P_tp,CPU_P_tp)
%[H,P] = kstest2(CPU_P_tp,PCR_P_tp)
%[H,P] = kstest2(PCR_P_tp,CPD_P_tp)

