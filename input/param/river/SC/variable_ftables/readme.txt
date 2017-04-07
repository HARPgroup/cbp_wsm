Reservoir F-Table Guidelines

(all transiton dates assume midnight, 0000, the beginning of that day)

--AS OF 11/03/03, 11 RESERVOIR FTABLES ARE INCOMPLETE DUE TO LACK OF SUFFICIENT DATA.  7 HAVE MODEL CALIBRATION POINTS AT THE MOUTH OF THEIR RESPECTIVE SEGMENT.  THE OTHER 4 HAVE NO CALIBRATION POINT DOWNSTREAM OR HAVE AT LEAST ONE SEGMENT BETWEEN THEM AND A CALIBRATION POINT.  COMMENTS FOLLOW:
  *Cal?*	*CAT*	        *DAM_NAME*	                 *STATE* *COMMENTS*
--Cal.P.	SU2_0030_0140	Otsego Lake	                 NY	City of Cooperstown, NY  Couldn't even get a contact, so probably not going to get much on this.  Create proxy based on size?		
--Cal.P.	SJ4_2060_2010	Warrior Ridge	                 PA	American Hydropower.  Waiting for data.
--Cal.P.	XU2_4070_4330	Brighton / Tridelphia L	         MD	No contacts, no data.  Create size-based proxy?
--Cal.P.	XU2_4330_4480	Rocky Gorge / Duckett	         MD	have data now, single ftable
--Cal.P.	PU1_4840_4760	Stony River Dam / Mt. Storm Pwr. WV	No contacts, no data.  Create size-based proxy?
--Cal.P.	JA5_7480_0001	Brasfield / L. Chesdin	         VA	STS Hydropower.  Waiting on data.
--Cal.P.	OD2_8920_8830	Lake Hyco	                 NC	No contacts, no data.  Lake Hyco is not even the terminal reservoir in its segment.  Probably difficult to get an FTable right because of that.
--No		GY0_4240_3951	Deep Creek	                 MD	Non-Bay Md (Western Panhandle).  Pa. Electric?  No data.
--No		OR4_7940_8270	Smith Mountain	                 VA	**
--No		OR4_8270_8120	Leesville	                 VA	**
--No		NR6_8500_7820	Claytor	                         VA	**
**Smith Mountain, Leesville, and Claytor are LARGE important reservoirs in non-Bay Va.  All owned by Appalacian Power Co.  Contacts made, but waiting on data.  It would be good to try to get these right, but who knows when the data will arrive.  

size-based proxy as follows:
GY0_4240_3951 used JA0_7291_7290 standard ftable
PU1_4840_4760 used JA0_7291_7290 standard ftable
SU2_0030_0140 used SU2_0291_0320 variable ftable
XU2_???? USGS has modeled these previously.  We should have the data somewhere. use proxies for now
    4070_4330 use JB2_7800_0001
    4330_4480 use SU2_0291_0320
OD2_8920_8830 used OD2_8560_8630
SJ4_2060_2010 used SJ4_2360_2340
OR4_7940_8270 OR4_8270_8120 JA5_7480_0001 - no close proxies, use SJ4_2360_2340 as the closest

***** some are calibration stations
XU2_4070_4330
XU2_4330_4480
PU1_4840_4760
SJ4_2060_2010
OD2_8920_8830

JA5_7480_0001, simple ftable, got this one 12/16/2003
NR6_8500_7820, simple ftable, got this one 12/16/2003

--PER INSTRUCTIONS, WE DID NOT BUILD FTABLES HERE IN Va FOR THE FOLLOWING THREE TERMINAL SUSQUEHANA DAMS
SL9_2520_2700	Safe Harbor	                         PA
SL9_2700_2720	Holtwood / McCalls Ferry	         PA
SL9_2720_0001	Conowingo	                         MD


North Anna (YP2_6390_6330)
--NO SEASONAL CHANGES.
--Stage-Discharge relationships not derived but provided directly by the cooperator.
---- CBP NOTES - just transferred ftable

Alvin R. Bush (SW3_1130_1390)
--NO SEASONAL CHANGES.  Simple, derived L-Q curve.
---- CBP NOTES - just transferred ftable

Cowanesque (SU2_0741_0690)  "W"
--TARGET CHANGE AT 1990.  No seasonal changes.  Try gradual 6-month transiton period for now.
--DISCH1: Ends Nov1, 1989.
--DISCH2: Begins Jun1, 1990.
---- CBP NOTES - make regular looking variable ftable, then write code to switch 

Curwensville (SW4_1860_1720) "R"
--2 seasons with transition periods until mid-1997, then constant year-round at former summer pool elevation.
--DISCH1: Winter, Dec1 - Apr23 (until Apr23, 1997)
--DISCH2: Summer, May8 - Nov15, AND year-round beginning May8, 1997 
---- CBP NOTES - standard variable format, need to write exception

East Sydney (SU2_0291_0320)
--2 seasons with transition periods
--DISCH1: Winter, Dec12 - Apr25 
--DISCH2: Summer, May15 - Nov28
---- CBP NOTES - standard variable format

T. Nelson Elliott / Broad Run (PL0_5141_5140)
--NO SEASONAL CHANGES.  Simple.
---- CBP NOTES - just transferred ftable

Flannagan (BS4_8540_8441)
--2 seasons with transition periods
--DISCH1: Winter, Dec3 - Mar25
--DISCH2: Summer, Apr25 - Oct1
---- CBP NOTES - standard variable format

Gathright/L.Moomaw (JU3_6900_6950)
--1 winter season and 5 months of "relatively constant discharge."  Transition occurs before and after winter season (disch1), but no transition occurs between summer months.  Therfore, transitions occur Dec1 - Jan19 and Jun5 - Jul1.
--DISCH1: Winter, Jan19 - Jun5
--DISCH2: Jul1 - Aug1
--DISCH3: Aug1 - Sep1
--DISCH4: Sep1 - Oct1
--DISCH5: Oct1 - Nov1
--DISCH6: Nov1 - Dec1
---- CBP NOTES - standard variable format  - made a new discharge column specifially for Jan1

Jennings Randolph/Bloomington Dam (PU3_4450_4440)
--2 seasons with transition periods
--DISCH1: Fall-Winter, Oct15 - Dec1
--DISCH2: Spring-Summ, Apr1 - Jul1
---- CBP NOTES - standard variable format  - made a new discharge column specifially for Jan1

Kerr (OR7_8470_8490)
--2 short seasons with long transition periods
--DISCH1: Winter, Dec1 - Feb1
--DISCH2: Spring, Apr1 - Jun1
---- CBP NOTES - standard variable format

Liberty (WM0_3881_3880)
--NO SEASONAL CHANGES.  Simple.
---- CBP NOTES - just transferred ftable

Loch Raven (WU3_3480_3481)
--NO SEASONAL CHANGES.  Simple.
---- CBP NOTES - just transferred ftable

L.Marburg (SL0_2831_2830)
--NO SEASONAL CHANGES.  Simple.
---- CBP NOTES - just transferred ftable

Lake Meade (JB1_8090_0001)
--NO SEASONAL CHANGES.  Simple.
---- CBP NOTES - just transferred ftable

Upper Occoquan (PL0_5250_0001)
--NO SEASONAL CHANGES.  Simple.
---- CBP NOTES - just transferred ftable

Philpott (OD2_8560_8630)
--2 short seasons with long transition periods
--DISCH1: Spring, Mar10 - Jul10
--DISCH2: Autumn, Oct1 - Dec1
---- CBP NOTES - standard variable format  - made a new discharge column specifially for Jan1

Prettyboy (WU0_3021_3020)
--NO SEASONAL CHANGES.  Simple.
---- CBP NOTES - just transferred ftable

Raystown (SJ4_2360_2340)
--2 seasons: only min Q changes, no pool change.  No transition period.
--DISCH1: Winter, Nov15 - May15
--DISCH2: Summer, May15 - Nov15 
---- CBP NOTES - standard variable format  

South Rivanna (JL2_6441_6520)
--NO SEASONAL CHANGES.  Simple.
---- CBP NOTES - just transferred ftable

Savage River (PU1_4190_4300)
--2 seasons with transition periods
--DISCH1: Winter, Nov1 - Feb1
--DISCH2: Summer, Apr15 - Jun15
---- CBP NOTES - standard variable format  

F.J. Sayers/Blanchard (SW3_1690_1660) "B"
--Rule curve change in Jul of 1994!  2 seasons before, one extra pool period added after.  Transitions periods apply.
--Prior to Jul 1, 1994:
  --DISCH1: May15 - Oct1
  --DISCH2: Not used
  --DISCH3: Dec1 - Apr1
--After Jul1, 1994:
  --DISCH1: May15 - Nov15
  --DISCH2: Dec1 - Feb15
  --DISCH3: Mar1 - Apr1
---- CBP NOTES - double standard variable format.  write special code B  (two ftables)  Not dealing well with march/april 1994

George B. Stevenson (SW3_1091_1380)
--NO SEASONAL CHANGES.  Simple, derived L-Q curve.
---- CBP NOTES - just transferred ftable
----  problem with table -- the first two lines have zero area and discharge.
Get rid of second line.

Swift Creek Reservoir (JA0_7291_7290)
--NO SEASONAL CHANGES.  Simple.
---- CBP NOTES - just transferred ftable

Tioga-Hammond (SU3_0831_0790)
--1 season, simple.  Gross simplification of operations at these semi-independent reservoirs--modelled now as one reservoir based on Tioga elevation and outflow and a combined volume/surf.area.  Simplification assumes that the head at Hammond and Tioga operate together, which they don't.  Still, it's better than nothing.
---- CBP NOTES - just transferred ftable

Western Branch (JB2_7800_0001)
--NO SEASONAL CHANGES.  Simple.
---- CBP NOTES - just transferred ftable

Whitney Point (SU3_0240_0350)
--2 seasons with trasition periods
--DISCH1: Winter, Dec10 - May5
--DISCH2: Summer, May15 - Dec1
---- CBP NOTES - standard variable format  
