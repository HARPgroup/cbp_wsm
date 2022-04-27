// ** Author: Gopal Bhatt ( Gopal.Bhatt@PSU.edu )          ** //

// ** Program updates LZETP parameter values in the PWATER ** //
// ** parameters table for the future CO2 condition.       ** //

// ** Adjustment provides a mechanism to mimic expected    ** //
// ** behavior of stomata at elevated CO2 conditions.      ** //

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cmath>

#define DEBUG if(1)

using namespace std;

int main()
{
	string S_ParamFolder = "../../../../input/param";

	string S_InpParamFolder, S_OutParamFolder, S_Landuse;
	string S_MonthlyAvgTempFileName;
	string S_MonthlyAvgWindFileName;

	int I_VLEFG_Column          = 10;
	int I_ALZETP_Column         = 30;
	int I_MLZETP_Columns[12]    = {67,68,69,70,71,72,73,74,75,76,77,78};

	double D_MonthlyAvgTemp[12]; // = {2.51578,3.33256,6.72464,12.6179,17.4525,22.6993,25.7243,24.1128,20.3833,14.1059,8.45194,4.1887};
	double D_MonthlyAvgTemp1st[12];
	double D_MonthlyAvgWind[12]; // = {11.9827,11.5599,11.1473,9.96213,8.90374,7.94488,7.64171,7.40302,9.06763,9.65807,11.337,11.4594};
	double D_MonthlyAvgWind1st[12];
	double D_LZETP[12]          = {-9.99};

	double D_Elevation;
	double D_PETfactor, D_Gamma, D_Delta, D_ra;
	double D_rc0, D_rc1;
	double D_z2mFactor;

	double D_RefCO2, D_PrjCO2;

        string S_InpPWATERFileName, S_OutPWATERFileName, S_InpSNOWFileName;
        ifstream IF_InpPWATERFile;
        ofstream OF_OutPWATERFile;
        ifstream IF_InpSNOWFile, IF_MonthlyAvgTempFile, IF_MonthlyAvgWindFile;




        DEBUG cout << "\nEnter Input Parameter Folder  : "; cin >> S_InpParamFolder;
        DEBUG cout << "\nEnter Output Parameter Folder : "; cin >> S_OutParamFolder;
        DEBUG cout << "\nEnter Land Use                : "; cin >> S_Landuse;

	DEBUG cout << "\nEnter Monthly Avg Temperature : "; cin >> S_MonthlyAvgTempFileName;
	DEBUG cout << "\nEnter Monthly Avg Wind        : "; cin >> S_MonthlyAvgWindFileName;

	DEBUG cout << "\nEnter Reference CO2           : "; cin >> D_RefCO2;
	DEBUG cout << "\nEnter Projected CO2           : "; cin >> D_PrjCO2;




        D_rc0 = 70; // m/s
        D_rc1 = D_rc0 / ( 1.4 - 0.4 * D_PrjCO2 / D_RefCO2 );
        D_z2mFactor = 4.87 / log ( 67.8 * 10 - 5.42 );
        DEBUG cout << "\nD_rc0 = " << D_rc0 << " D_rc1 = " << D_rc1 << " D_z2mFactor = " << D_z2mFactor;


	S_InpPWATERFileName = S_ParamFolder + "/" + S_Landuse + "/" + S_InpParamFolder + "/PWATER.csv";
	S_InpSNOWFileName   = S_ParamFolder + "/" + S_Landuse + "/" + S_InpParamFolder + "/SNOW.csv";
	S_OutPWATERFileName = S_ParamFolder + "/" + S_Landuse + "/" + S_OutParamFolder + "/PWATER.csv";

	IF_InpPWATERFile.open(S_InpPWATERFileName.c_str());
	if ( ! IF_InpPWATERFile.good() )
	{
		cout << "Unable to open file " << S_InpPWATERFileName;
		return -1;
	}

	OF_OutPWATERFile.open(S_OutPWATERFileName.c_str());
        if ( ! OF_OutPWATERFile.good() )
        {
                cout << "Unable to open file " << S_OutPWATERFileName;
                return -1;
        }


	string S_OneLine;
	getline(IF_InpPWATERFile,S_OneLine); // Header Line 1
	OF_OutPWATERFile << S_OneLine << "\n";

	getline(IF_InpPWATERFile,S_OneLine); // Header Line 2
	OF_OutPWATERFile << S_OneLine << "\n";

	string S_PWATER[85], S_Elevation;
	while ( getline(IF_InpPWATERFile, S_OneLine ) )
	{
		if ( S_OneLine.substr(0,3) == "end" )
		{
			OF_OutPWATERFile << S_OneLine;
			break;
		}

		std::stringstream SS_LineStream(S_OneLine);
		for ( int col=0; col<85; col++ )
		{
                	S_PWATER[col] = "";
                	getline(SS_LineStream, S_PWATER[col], ',');
		}

                // ** IF CONSTANT LZETP WAS USED THEN ASSISGN TO ALL MONTHS
                if ( S_PWATER[I_VLEFG_Column-1] == "0" )
                {
                        for( int mon = 0; mon < 12; mon++ ) 
                        {
                                S_PWATER[ I_MLZETP_Columns[mon] - 1 ] = S_PWATER[I_ALZETP_Column-1];
                        }
                }

		// Do processing for the land segment




		IF_InpSNOWFile.open(S_InpSNOWFileName.c_str());
	        if ( ! IF_InpSNOWFile.good() )
        	{
                	cout << "Unable to open file " << S_InpSNOWFileName;
                	return -1;
        	}

		while ( getline(IF_InpSNOWFile, S_OneLine) )
		{
			std::stringstream SS_LineStream(S_OneLine);
			S_Elevation = "";
			getline(SS_LineStream, S_Elevation, ','); // lseg
			if ( S_Elevation == S_PWATER[0] )
			{
				getline(SS_LineStream, S_Elevation, ','); // flag
				getline(SS_LineStream, S_Elevation, ','); // lat
				getline(SS_LineStream, S_Elevation, ','); // elevation
				break;
			}
		}
		IF_InpSNOWFile.close();
		D_Elevation = atof ( S_Elevation.c_str() ) * 0.3048; // feet to meters




		IF_MonthlyAvgTempFile.open(S_MonthlyAvgTempFileName.c_str());
		if ( ! IF_MonthlyAvgTempFile.good() )
		{
			cout << "\nUnable to open file " << S_MonthlyAvgTempFileName << "\n";
			return -1;
		}

		DEBUG cout << "\nTemp   ";
		while ( getline(IF_MonthlyAvgTempFile, S_OneLine) )
		{
			std::stringstream SS_LineStream(S_OneLine);
			string S_string;
			getline(SS_LineStream, S_string, ',');
			if ( S_string == S_PWATER[0] )
			{
				for ( int mon = 0; mon < 12; mon++ )
				{
					getline(SS_LineStream, S_string, ',');
					D_MonthlyAvgTemp[mon] = atof (S_string.c_str());
					DEBUG cout << " ," << D_MonthlyAvgTemp[mon];
				}
				break;
			}
		}
		IF_MonthlyAvgTempFile.close();
		DEBUG cout << "\nTemp1st";
		for ( int mon = 0; mon < 12; mon++ )
		{
			D_MonthlyAvgTemp1st[mon] = ( D_MonthlyAvgTemp[mon] + D_MonthlyAvgTemp[ (11+mon) % 12 ] ) / 2;
			DEBUG cout << " ," << D_MonthlyAvgTemp1st[mon];
		}




		IF_MonthlyAvgWindFile.open(S_MonthlyAvgWindFileName.c_str());
		if ( ! IF_MonthlyAvgWindFile.good() )
		{
			cout << "\nUnable to open file " << S_MonthlyAvgWindFileName << "\n";
			return -1;
		}

		while ( getline(IF_MonthlyAvgWindFile, S_OneLine) )
		{
			std::stringstream SS_LineStream(S_OneLine);
			string S_string;
			getline(SS_LineStream, S_string, ',');
			if ( S_string == S_PWATER[0] )
			{
				for ( int mon = 0; mon < 12; mon++ )
				{
					getline(SS_LineStream, S_string, ',');
					D_MonthlyAvgWind[mon] = atof (S_string.c_str());
				}
				break;
			}
		}
		IF_MonthlyAvgWindFile.close();
		DEBUG cout << "\nWind1st   ";
		for ( int mon = 0; mon < 12; mon++ )
		{
			D_MonthlyAvgWind1st[mon] = ( ( D_MonthlyAvgWind[mon] + D_MonthlyAvgWind[ (11+mon) % 12] ) / 2 ) * D_z2mFactor * ( 1609.344 / 3600 );
			cout << ", " << D_MonthlyAvgWind1st[mon];
		}




		cout << "\n" << S_PWATER[0] << "\t" << D_Elevation;

		DEBUG cout << "\n" << S_PWATER[0];
		D_Gamma = 0.000665 * 101.3 * pow ( ( ( 293 - 0.0065 * D_Elevation ) / 293 ), 5.26 );
		for ( int mon = 0; mon < 12; mon++ )
		{
			D_Delta     = 4098.0 * ( 0.6108 * exp ( 17.27 * D_MonthlyAvgTemp1st[mon] / ( D_MonthlyAvgTemp1st[mon] + 237.3 ) ) / pow ( D_MonthlyAvgTemp1st[mon] + 237.3, 2 ) );
			D_ra        = 208.0 / D_MonthlyAvgWind1st[mon]; // ( ( D_Delta + D_Gamma ) * D_ra )
			D_PETfactor = ( ( ( D_Delta + D_Gamma ) * D_ra ) + D_Gamma * D_rc0 ) / ( ( ( D_Delta + D_Gamma ) * D_ra ) + D_Gamma * D_rc1 );
			DEBUG cout << "\t(" << D_PETfactor<<","<<S_PWATER[ I_MLZETP_Columns[mon] - 1 ];
			std::ostringstream SS_temp;
			if ( D_PETfactor > 0 )
			{
				SS_temp << ( ( 1- (1 - atof ( S_PWATER[ I_MLZETP_Columns[mon] - 1 ].c_str())) / D_PETfactor ) > 0.01 ? ( 1- (1 - atof ( S_PWATER[ I_MLZETP_Columns[mon] - 1 ].c_str())) / D_PETfactor ) : 0.01 );
			}
			else
				SS_temp << 0;
			//SS_temp << D_PETfactor * atof ( S_PWATER[ I_MLZETP_Columns[mon] - 1 ].c_str() );
			S_PWATER[ I_MLZETP_Columns[mon] - 1 ] = SS_temp.str();
			DEBUG cout << "," << S_PWATER[ I_MLZETP_Columns[mon] - 1 ] << ")";
		}


		S_PWATER[I_VLEFG_Column-1]  = "1";
		S_PWATER[I_ALZETP_Column-1] = "NA";

		// Write the parameter line of the land segment
		for ( int col=0; col<85; col++ )
                {
			OF_OutPWATERFile << S_PWATER[col] << ",";
                }
		OF_OutPWATERFile << "\n";
	}


	

	for (int i=0; i<12; i++)
		cout << I_MLZETP_Columns[i] << "\t";

	IF_InpPWATERFile.close();
	OF_OutPWATERFile.close();
	cout << "\n\n";
}