echo BS3_8350_8330 | find_loads.exe >/model/all_loads.csv
echo BS4_8440_0003 | find_loads.exe >>/model/all_loads.csv
echo BS4_8540_8441 | find_loads.exe >>/model/all_loads.csv
echo EL0_4562_0003 | find_loads.exe >>/model/all_loads.csv
echo EL1_5430_0001 | find_loads.exe >>/model/all_loads.csv
echo EL2_4400_4590 | find_loads.exe >>/model/all_loads.csv
echo EL2_5110_5270 | find_loads.exe >>/model/all_loads.csv
echo EM2_3980_0001 | find_loads.exe >>/model/all_loads.csv
echo GY0_3800_3801 | find_loads.exe >>/model/all_loads.csv
echo GY0_3950_3952 | find_loads.exe >>/model/all_loads.csv
echo JA1_7600_7570 | find_loads.exe >>/model/all_loads.csv
echo JA2_7550_7280 | find_loads.exe >>/model/all_loads.csv
echo JA5_7480_0001 | find_loads.exe >>/model/all_loads.csv
echo JB3_6820_7053 | find_loads.exe >>/model/all_loads.csv
echo JL1_6560_6440 | find_loads.exe >>/model/all_loads.csv
echo JL1_6770_6850 | find_loads.exe >>/model/all_loads.csv
echo JL1_6940_7200 | find_loads.exe >>/model/all_loads.csv
echo JL1_7080_7190 | find_loads.exe >>/model/all_loads.csv
echo JL2_6240_6520 | find_loads.exe >>/model/all_loads.csv
echo JL2_6441_6520 | find_loads.exe >>/model/all_loads.csv
echo JL2_7110_7120 | find_loads.exe >>/model/all_loads.csv
echo JL2_7240_7350 | find_loads.exe >>/model/all_loads.csv
echo JL4_6520_6710 | find_loads.exe >>/model/all_loads.csv
echo JL6_6890_6990 | find_loads.exe >>/model/all_loads.csv
echo JL6_7430_7320 | find_loads.exe >>/model/all_loads.csv
echo JL7_6800_7070 | find_loads.exe >>/model/all_loads.csv
echo JL7_7100_7030 | find_loads.exe >>/model/all_loads.csv
echo JU1_6300_6650 | find_loads.exe >>/model/all_loads.csv
echo JU1_7630_7490 | find_loads.exe >>/model/all_loads.csv
echo JU1_7750_7560 | find_loads.exe >>/model/all_loads.csv
echo JU2_6410_6640 | find_loads.exe >>/model/all_loads.csv
echo JU2_6600_6810 | find_loads.exe >>/model/all_loads.csv
echo JU3_6380_6900 | find_loads.exe >>/model/all_loads.csv
echo JU3_6640_6790 | find_loads.exe >>/model/all_loads.csv
echo JU3_6650_7300 | find_loads.exe >>/model/all_loads.csv
echo JU3_6900_6950 | find_loads.exe >>/model/all_loads.csv
echo JU4_7260_0003 | find_loads.exe >>/model/all_loads.csv
echo JU4_7330_0003 | find_loads.exe >>/model/all_loads.csv
echo JU5_7300_0003 | find_loads.exe >>/model/all_loads.csv
echo JU5_7500_7420 | find_loads.exe >>/model/all_loads.csv
echo MN3_7540_7680 | find_loads.exe >>/model/all_loads.csv
echo MN3_7930_8010 | find_loads.exe >>/model/all_loads.csv
echo MN4_8260_8400 | find_loads.exe >>/model/all_loads.csv
echo MN5_8161_0003 | find_loads.exe >>/model/all_loads.csv
echo NR2_8210_8180 | find_loads.exe >>/model/all_loads.csv
echo NR2_8600_8700 | find_loads.exe >>/model/all_loads.csv
echo NR3_8420_8430 | find_loads.exe >>/model/all_loads.csv
echo NR5_8870_0003 | find_loads.exe >>/model/all_loads.csv
echo NR6_8051_8000 | find_loads.exe >>/model/all_loads.csv
echo NR6_8500_0003 | find_loads.exe >>/model/all_loads.csv
echo OD1_8910_8930 | find_loads.exe >>/model/all_loads.csv
echo OD2_8560_8630 | find_loads.exe >>/model/all_loads.csv
echo OD2_8670_8890 | find_loads.exe >>/model/all_loads.csv
echo OD3_8850_8931 | find_loads.exe >>/model/all_loads.csv
echo OR1_8320_8271 | find_loads.exe >>/model/all_loads.csv
echo OR2_7650_8070 | find_loads.exe >>/model/all_loads.csv
echo OR2_7670_7840 | find_loads.exe >>/model/all_loads.csv
echo OR2_8020_8130 | find_loads.exe >>/model/all_loads.csv
echo OR2_8460_8271 | find_loads.exe >>/model/all_loads.csv
echo OR4_8120_7890 | find_loads.exe >>/model/all_loads.csv
echo OR5_8200_8370 | find_loads.exe >>/model/all_loads.csv
echo PL0_4510_0001 | find_loads.exe >>/model/all_loads.csv
echo PL0_5000_0001 | find_loads.exe >>/model/all_loads.csv
echo PL0_5010_5130 | find_loads.exe >>/model/all_loads.csv
echo PL0_5510_0001 | find_loads.exe >>/model/all_loads.csv
echo PL0_5530_5710 | find_loads.exe >>/model/all_loads.csv
echo PL0_5540_5490 | find_loads.exe >>/model/all_loads.csv
echo PL0_5720_0001 | find_loads.exe >>/model/all_loads.csv
echo PL0_5730_5690 | find_loads.exe >>/model/all_loads.csv
echo PL0_5750_0001 | find_loads.exe >>/model/all_loads.csv
echo PL0_5830_0001 | find_loads.exe >>/model/all_loads.csv
echo PL1_4460_4780 | find_loads.exe >>/model/all_loads.csv
echo PL1_4540_0001 | find_loads.exe >>/model/all_loads.csv
echo PL1_5370_5470 | find_loads.exe >>/model/all_loads.csv
echo PL1_5910_0001 | find_loads.exe >>/model/all_loads.csv
echo PM1_3120_3400 | find_loads.exe >>/model/all_loads.csv
echo PM1_3510_4000 | find_loads.exe >>/model/all_loads.csv
echo PM1_4250_4500 | find_loads.exe >>/model/all_loads.csv
echo PM1_4430_4200 | find_loads.exe >>/model/all_loads.csv
echo PM2_2860_3040 | find_loads.exe >>/model/all_loads.csv
echo PM2_4860_4670 | find_loads.exe >>/model/all_loads.csv
echo PM3_4670_4660 | find_loads.exe >>/model/all_loads.csv
echo PM4_4040_0003 | find_loads.exe >>/model/all_loads.csv
echo PM7_4200_0003 | find_loads.exe >>/model/all_loads.csv
echo PM7_4820_0001 | find_loads.exe >>/model/all_loads.csv
echo PS0_6150_6160 | find_loads.exe >>/model/all_loads.csv
echo PS1_4790_4830 | find_loads.exe >>/model/all_loads.csv
echo PS2_5550_5560 | find_loads.exe >>/model/all_loads.csv
echo PS2_5560_5100 | find_loads.exe >>/model/all_loads.csv
echo PS2_6490_6420 | find_loads.exe >>/model/all_loads.csv
echo PS2_6730_6660 | find_loads.exe >>/model/all_loads.csv
echo PS3_5100_5080 | find_loads.exe >>/model/all_loads.csv
echo PS3_6161_6280 | find_loads.exe >>/model/all_loads.csv
echo PS3_6460_6230 | find_loads.exe >>/model/all_loads.csv
echo PS4_5840_5240 | find_loads.exe >>/model/all_loads.csv
echo PS4_6360_5840 | find_loads.exe >>/model/all_loads.csv
echo PS5_4380_4370 | find_loads.exe >>/model/all_loads.csv
echo PS5_5240_5200 | find_loads.exe >>/model/all_loads.csv
echo PU0_3000_3090 | find_loads.exe >>/model/all_loads.csv
echo PU1_3030_3440 | find_loads.exe >>/model/all_loads.csv
echo PU1_3170_3580 | find_loads.exe >>/model/all_loads.csv
echo PU1_3940_3970 | find_loads.exe >>/model/all_loads.csv
echo PU1_4190_4300 | find_loads.exe >>/model/all_loads.csv
echo PU1_4840_4760 | find_loads.exe >>/model/all_loads.csv
echo PU2_2840_3080 | find_loads.exe >>/model/all_loads.csv
echo PU2_3090_4050 | find_loads.exe >>/model/all_loads.csv
echo PU2_3140_3680 | find_loads.exe >>/model/all_loads.csv
echo PU2_3180_3370 | find_loads.exe >>/model/all_loads.csv
echo PU2_4220_3900 | find_loads.exe >>/model/all_loads.csv
echo PU2_4360_4160 | find_loads.exe >>/model/all_loads.csv
echo PU2_4730_4220 | find_loads.exe >>/model/all_loads.csv
echo PU2_5190_4310 | find_loads.exe >>/model/all_loads.csv
echo PU2_6050_5190 | find_loads.exe >>/model/all_loads.csv
echo PU3_3290_3390 | find_loads.exe >>/model/all_loads.csv
echo PU3_3680_3890 | find_loads.exe >>/model/all_loads.csv
echo PU3_3860_3610 | find_loads.exe >>/model/all_loads.csv
echo PU4_3890_3990 | find_loads.exe >>/model/all_loads.csv
echo PU4_4310_4210 | find_loads.exe >>/model/all_loads.csv
echo PU4_4440_0003 | find_loads.exe >>/model/all_loads.csv
echo PU4_5050_0003 | find_loads.exe >>/model/all_loads.csv
echo PU6_3610_3530 | find_loads.exe >>/model/all_loads.csv
echo PU6_3752_4080 | find_loads.exe >>/model/all_loads.csv
echo PU6_4020_3870 | find_loads.exe >>/model/all_loads.csv
echo RL0_6540_0001 | find_loads.exe >>/model/all_loads.csv
echo RL1_6180_0001 | find_loads.exe >>/model/all_loads.csv
echo RU2_5940_6200 | find_loads.exe >>/model/all_loads.csv
echo RU2_6090_6220 | find_loads.exe >>/model/all_loads.csv
echo RU3_5610_0003 | find_loads.exe >>/model/all_loads.csv
echo RU3_6170_6040 | find_loads.exe >>/model/all_loads.csv
echo RU4_5640_0003 | find_loads.exe >>/model/all_loads.csv
echo RU5_6030_0001 | find_loads.exe >>/model/all_loads.csv
echo SJ2_2530_2820 | find_loads.exe >>/model/all_loads.csv
echo SJ3_2040_1980 | find_loads.exe >>/model/all_loads.csv
echo SJ3_2250_2230 | find_loads.exe >>/model/all_loads.csv
echo SJ4_2060_2010 | find_loads.exe >>/model/all_loads.csv
echo SJ4_2660_2360 | find_loads.exe >>/model/all_loads.csv
echo SJ5_2210_2320 | find_loads.exe >>/model/all_loads.csv
echo SJ6_2130_0003 | find_loads.exe >>/model/all_loads.csv
echo SL1_1730_1700 | find_loads.exe >>/model/all_loads.csv
echo SL1_2390_2420 | find_loads.exe >>/model/all_loads.csv
echo SL1_2770_2730 | find_loads.exe >>/model/all_loads.csv
echo SL1_2830_2760 | find_loads.exe >>/model/all_loads.csv
echo SL2_1810_2030 | find_loads.exe >>/model/all_loads.csv
echo SL2_1850_1990 | find_loads.exe >>/model/all_loads.csv
echo SL2_1990_2070 | find_loads.exe >>/model/all_loads.csv
echo SL2_2910_3060 | find_loads.exe >>/model/all_loads.csv
echo SL2_3060_0001 | find_loads.exe >>/model/all_loads.csv
echo SL3_1710_1740 | find_loads.exe >>/model/all_loads.csv
echo SL3_2290_2260 | find_loads.exe >>/model/all_loads.csv
echo SL3_2350_2470 | find_loads.exe >>/model/all_loads.csv
echo SL3_2400_2440 | find_loads.exe >>/model/all_loads.csv
echo SL3_2420_2700 | find_loads.exe >>/model/all_loads.csv
echo SL3_2460_2430 | find_loads.exe >>/model/all_loads.csv
echo SL3_2730_2550 | find_loads.exe >>/model/all_loads.csv
echo SL4_2140_2240 | find_loads.exe >>/model/all_loads.csv
echo SL4_2370_2330 | find_loads.exe >>/model/all_loads.csv
echo SL8_1760_1780 | find_loads.exe >>/model/all_loads.csv
echo SL9_2270_0003 | find_loads.exe >>/model/all_loads.csv
echo SL9_2490_2520 | find_loads.exe >>/model/all_loads.csv
echo SL9_2720_0001 | find_loads.exe >>/model/all_loads.csv
echo SU2_0670_0810 | find_loads.exe >>/model/all_loads.csv
echo SU2_0741_0690 | find_loads.exe >>/model/all_loads.csv
echo SU2_0900_0870 | find_loads.exe >>/model/all_loads.csv
echo SU2_0920_0830 | find_loads.exe >>/model/all_loads.csv
echo SU3_0710_0910 | find_loads.exe >>/model/all_loads.csv
echo SU3_0790_0770 | find_loads.exe >>/model/all_loads.csv
echo SU3_0810_0970 | find_loads.exe >>/model/all_loads.csv
echo SU3_0831_0790 | find_loads.exe >>/model/all_loads.csv
echo SU3_0970_1120 | find_loads.exe >>/model/all_loads.csv
echo SU4_0690_0650 | find_loads.exe >>/model/all_loads.csv
echo SU5_0340_0310 | find_loads.exe >>/model/all_loads.csv
echo SU5_0610_0600 | find_loads.exe >>/model/all_loads.csv
echo SU6_0480_0520 | find_loads.exe >>/model/all_loads.csv
echo SU7_0850_0730 | find_loads.exe >>/model/all_loads.csv
echo SU7_1120_1140 | find_loads.exe >>/model/all_loads.csv
echo SU8_1610_1530 | find_loads.exe >>/model/all_loads.csv
echo SW0_1520_1600 | find_loads.exe >>/model/all_loads.csv
echo SW1_1180_1190 | find_loads.exe >>/model/all_loads.csv
echo SW1_1450_1510 | find_loads.exe >>/model/all_loads.csv
echo SW1_1890_1830 | find_loads.exe >>/model/all_loads.csv
echo SW3_1040_1220 | find_loads.exe >>/model/all_loads.csv
echo SW3_1091_1380 | find_loads.exe >>/model/all_loads.csv
echo SW3_1130_1390 | find_loads.exe >>/model/all_loads.csv
echo SW3_1270_1370 | find_loads.exe >>/model/all_loads.csv
echo SW3_1690_0003 | find_loads.exe >>/model/all_loads.csv
echo SW3_1690_1660 | find_loads.exe >>/model/all_loads.csv
echo SW3_1870_1800 | find_loads.exe >>/model/all_loads.csv
echo SW3_1920_1750 | find_loads.exe >>/model/all_loads.csv
echo SW4_1110_1150 | find_loads.exe >>/model/all_loads.csv
echo SW4_1260_0003 | find_loads.exe >>/model/all_loads.csv
echo SW4_1430_1490 | find_loads.exe >>/model/all_loads.csv
echo SW4_1860_1720 | find_loads.exe >>/model/all_loads.csv
echo SW4_1940_1860 | find_loads.exe >>/model/all_loads.csv
echo SW5_1350_0003 | find_loads.exe >>/model/all_loads.csv
echo SW5_1540_0003 | find_loads.exe >>/model/all_loads.csv
echo SW6_1330_1230 | find_loads.exe >>/model/all_loads.csv
echo SW7_1320_0003 | find_loads.exe >>/model/all_loads.csv
echo SW7_1640_0003 | find_loads.exe >>/model/all_loads.csv
echo TU2_8790_9070 | find_loads.exe >>/model/all_loads.csv
echo TU2_8950_9040 | find_loads.exe >>/model/all_loads.csv
echo TU3_8650_8800 | find_loads.exe >>/model/all_loads.csv
echo TU3_9230_9260 | find_loads.exe >>/model/all_loads.csv
echo TU4_8680_8810 | find_loads.exe >>/model/all_loads.csv
echo WM1_3660_3910 | find_loads.exe >>/model/all_loads.csv
echo WM3_3880_4060 | find_loads.exe >>/model/all_loads.csv
echo WU1_3350_3490 | find_loads.exe >>/model/all_loads.csv
echo WU2_3020_3320 | find_loads.exe >>/model/all_loads.csv
echo XL0_5320_0001 | find_loads.exe >>/model/all_loads.csv
echo XL1_4690_0001 | find_loads.exe >>/model/all_loads.csv
echo XU0_4130_4070 | find_loads.exe >>/model/all_loads.csv
echo XU2_4270_0003 | find_loads.exe >>/model/all_loads.csv
echo XU2_4330_4480 | find_loads.exe >>/model/all_loads.csv
echo XU3_4650_0001 | find_loads.exe >>/model/all_loads.csv
echo YM2_6120_6430 | find_loads.exe >>/model/all_loads.csv
echo YM4_6620_0003 | find_loads.exe >>/model/all_loads.csv
echo YP0_6860_6840 | find_loads.exe >>/model/all_loads.csv
echo YP1_6570_6680 | find_loads.exe >>/model/all_loads.csv
echo YP2_6390_6330 | find_loads.exe >>/model/all_loads.csv
echo YP3_6330_6700 | find_loads.exe >>/model/all_loads.csv
echo YP4_6720_6750 | find_loads.exe >>/model/all_loads.csv
