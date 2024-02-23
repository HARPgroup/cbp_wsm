#!/bin/csh

df -h

set SCENARIOS = ( 1985PROGRA_20181122 2009PROGRA_20181122 2010E3AA_20181122 2010WIP2AA_20181122 2010WIP2AA_20190301 2017PROGAA_20190402 2017PROGRA_20181122 2025WIP2AA_20190301 ABCDEFGHIJKLMNOPQRSTUVWXYZAB AEIOU_N20180213 ALLFOREST_20181122 C25WIP21808T88P50R45P50Y C25WIP21903BXLXT88P50R45P50Y C25WIP21903BYLXT88P50R45P50Y C25WIP21903BYLYT88P50R45P50Y C35WIP1806H45P50R45P50Y C35WIP21903BYLXH45P50R45P50Y C35WIP21903BYLXS45P50R45P50Y C35WIP21903BYLYH45P50R45P50Y C35WIP21903BYLYS45P50R45P50Y C45WIP1806H45P50R45P50Y C45WIP21903BYLXH45P50R45P50Y C45WIP21903BYLXS45P50R45P50Y C45WIP21903BYLYH45P50R45P50Y C45WIP21903BYLYS45P50R45P50Y C50WIP21808R45P50R45P50Y C55WIP21903BYLXR45P50R45P50Y C55WIP21903BYLXS45P50R45P50Y C55WIP21903BYLYR45P50R45P50Y C55WIP21903BYLYS45P50R45P50Y CBASE1808L35CY35H45P50R45P50Y CBASE1808L45CY45H45P50R45P50Y CF2010WIP2AA_20180615 CFBASE30Y20180615R2 CFBASE30Y20180615R3 CXXWIP21808NOCNGENOCNGEN CXXWIP21903BXLXNOCNGENOCNGEN CXXWIP21903BXLYNOCNGENOCNGEN JCHLADWIP3Y3020190815 JCHLAVAMWAB20180808 JCHLAVAMWABD20180808 JCHLAVAMWABp20180808 JCHLAVAMWABY3020190815 JCHLAVAMWAC20180808 JCHLAVAMWAD20180808 JCHLAWIP2LOE20180808 MDDWIP3AA_20190402 MDDWIP3toPTAA_20190402 MFBASE1808C25BCSDR45M09 MFBASE1808C25BCSDR45P50 MFBASE1808C25BCSDR85M06 MFBASE1808C25BCSDR85M09 MFBASE1808C25BCSDR85M31 MFBASE1808C25BCSDR85P50 MFBASE1808C25MACAR85M06 MFBASE1808C25MACAR85M27 MFBASE1808C25MACAR85M31 MFBASE1808C25MACAR85P50 MFBASE1808C25T88YR45P50 MFBASE1808C50BCSDR45P50 MFBASE1808C50MACAR85M28 MFBASE1808CXXNONERXXMXX VADWIP3AA_20190719 VADWIP3toPTAA_20190719 xJCHLADWIP3Y3020190815 )


foreach SCENARIO ( $SCENARIOS )
   echo "... processing $SCENARIO"
   rm -r ../../output/eop/aveann/$SCENARIO 
   find ../../output/eor/aveann/$SCENARIO -name '*.ave' | xargs rm -f
   rm -r ../../output/bay/aveann/$SCENARIO
end

df -h
