% This program calculates the 2-vector, 2-tailed KS test to look for 
% statistical differences in the regional factors in the coastal plain
%
%  The numbers are calculated from the calibration spreadsheet '$scen ungaged basins.xls'
% 
% This version is for the p52 calibrations
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

clear all;
ES_A_tn = [ 0.77,0.5,0.764 ];

UW_A_tn = [ 0.976,1.15,2,0.851,1.151,0.5,1.403 ];

LW_A_tn = [ 0.801,0.536,0.87,0.5 ];

ES_A_tp = [ 0.5,0.5,0.5 ];

UW_A_tp = [ 0.5,0.716,0.668,1.82,0.681,0.969,1.004,1.828 ];

LW_A_tp = [ 0.5,0.5,0.938,1.065,0.745,0.947,0.5 ];

CPD_A_tn = [ 0.851, 1.403, 0.5  ];

CPU_A_tn = [ 0.536, 0.87, 0.5, 0.77, 0.5, 0.764, 0.5 ];

PCR_A_tn = [ 0.801, 1.151, 1.15, 1.403, 0.976, 2 ];

CPD_A_tp = [ 0.5, 1.004, 1.82, 1.828, 0.5 ];

CPU_A_tp = [ 0.938, 0.745, 0.969, 0.5, 0.5, 0.5, 0.5 ];

PCR_A_tp = [ 0.5, 0.5, 1.065, 0.681, 1.004, 0.716, 1.828, 0.5, 0.668, 0.947 ];

CP_tn = [ 0.851, 1.403, 0.5, 0.536, 0.87, 0.5, 0.77, 0.5, 0.764, 0.5 ];

CP_tp = [ 0.5, 1.004, 1.82, 1.828, 0.5, 0.938, 0.745, 0.969, 0.5, 0.5, 0.5, 0.5 ];

[H,P] = kstest2(ES_A_tn,UW_A_tn)
[H,P] = kstest2(UW_A_tn,LW_A_tn)
[H,P] = kstest2(LW_A_tn,ES_A_tn)

[H,P] = kstest2(ES_A_tp,UW_A_tp)
[H,P] = kstest2(UW_A_tp,LW_A_tp)
[H,P] = kstest2(LW_A_tp,ES_A_tp)

[H,P] = kstest2(CPD_A_tn,CPU_A_tn)
[H,P] = kstest2(CPU_A_tn,PCR_A_tn)
[H,P] = kstest2(PCR_A_tn,CPD_A_tn)

[H,P] = kstest2(CPD_A_tp,CPU_A_tp)
[H,P] = kstest2(CPU_A_tp,PCR_A_tp)
[H,P] = kstest2(PCR_A_tp,CPD_A_tp)

[H,P] = kstest2(PCR_A_tn,CP_tn)
[H,P] = kstest2(PCR_A_tp,CP_tp)

