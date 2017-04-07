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
ES_A_tn = [ 0.77, 0.5, 0.738 ];

UW_A_tn = [ 0.981, 1.086, 1.861, 0.686, 1.151, 0.5, 2 ];

LW_A_tn = [ 0.802, 0.5, 0.7, 0.5 ];

ES_A_tp = [ 0.5, 0.5, 0.5 ];

UW_A_tp = [ 0.5, 0.638, 0.589, 1.79, 0.658, 0.969, 0.98, 2 ];

LW_A_tp = [ 0.5, 0.5, 0.93, 1.034, 0.741, 0.943, 0.5 ];

CPD_A_tn = [ 0.686, 2, 0.5 ];

CPU_A_tn = [ 0.5, 0.7, 0.5, 0.77, 0.5, 0.738, 0.5 ];

PCR_A_tn = [ 0.802, 1.151, 1.086, 2, 0.981, 1.861 ];

CPD_A_tp = [ 0.5, 0.98, 1.79, 2, 0.5 ];

CPU_A_tp = [ 0.93, 0.741, 0.969, 0.5, 0.5, 0.5, 0.5 ];

PCR_A_tp = [ 0.5, 0.5, 1.034, 0.658, 0.98, 0.638, 2, 0.5, 0.589, 0.943 ];

CP_tn = [0.686, 2, 0.5, 0.5, 0.7, 0.5, 0.77, 0.5, 0.738, 0.5];

CP_tp = [ 0.5, 0.98, 1.79, 2, 0.5, 0.93, 0.741, 0.969, 0.5, 0.5, 0.5, 0.5 ];

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

