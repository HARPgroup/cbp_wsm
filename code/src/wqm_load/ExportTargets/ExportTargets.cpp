#include <iostream>
#include <fstream>
#include <string>
#include <sstream>


#define DEBUG    if(1)
#define MAXLUS   50
#define MAXRIVS  2000
#define MAXLANDS 500
#define MAXAPPS  20

using namespace std;


string S_prefix = "../../../";

int _Read_D_sensitivity(char* landuse, string S_InputFile, string S_species, int I_nSources, char C_source[][254], double *D_sensitivity)
{
	DEBUG cout << "\nRead sensitivies of landuse  : " << landuse;


	for (int I_nSource = 0; I_nSource < I_nSources; I_nSource++)
		D_sensitivity[I_nSource] = 0.0;
	

	char oneLine[2540];
	char tempC[254];
	string S_data, S_oneline;

	ifstream IF_sensitivitiesFile;
	string   S_sensitivitiesFile;
	S_sensitivitiesFile = S_prefix + "/code/src/ExportTargets/"+S_InputFile+"/" + "sensitivity_" + S_species + ".dat";

	IF_sensitivitiesFile.open( S_sensitivitiesFile.c_str() );
        if ( ! IF_sensitivitiesFile.good() )
        {       
                cout << "Unable to open file: " << S_sensitivitiesFile << "\n";
                return -1;
        }

	getline(IF_sensitivitiesFile, S_oneline);
        std::stringstream headerstream(S_oneline);
	stringstream sourcestream;
        while ( getline(headerstream, S_data, ',') )
                sourcestream << S_data << " ";
        sourcestream >> S_data;



	while ( getline(IF_sensitivitiesFile, S_oneline) )
        {
                //cout << " S_oneline= " << S_oneline << "\n";
                stringstream landusestream;
                stringstream sensitivitystream;
                landusestream << S_oneline;
                //while ( getline(linestream2, S_data, ',') )
                while ( landusestream.getline(tempC,256,',') )
                {
                        //cout << "%" << tempC << " ";
                        sensitivitystream << tempC << " ";
                }
                sensitivitystream >> S_data;
                //cout << " * " << S_data << " ";
                if ( strcmp ( S_data.c_str(), landuse ) == 0 )
                {
			while ( sourcestream >> S_data )
			{
	                        for (int I_nSource = 0; I_nSource < I_nSources; I_nSource++)
				{
					if ( strcmp (S_data.c_str(), C_source[I_nSource]) == 0 )
					{
	                                	sensitivitystream >> D_sensitivity[I_nSource];
						break;
					}
				}
			}
                        //cout << "\n" << landuse;
                        //for (int I_nSource = 0; I_nSource < *I_nSources; I_nSource++)
                                //cout << "\t" << I_source[I_nSource];

			cout << " ~ " << landuse;
			for (int I_nSource = 0; I_nSource < I_nSources; I_nSource++)
				cout << ", " << C_source[I_nSource] << " " << D_sensitivity[I_nSource];

                        IF_sensitivitiesFile.close();
                        return 0;
                }
        }


	cout << " ~ " << landuse;
	for (int I_nSource = 0; I_nSource < I_nSources; I_nSource++)
                cout << ",\t" << D_sensitivity[I_nSource];


	IF_sensitivitiesFile.close();
	return 0;
}

int _Read_I_sources(string S_InputFile, char* landuse, int *I_nSources, char C_source[][254], int *I_source)
{
	DEBUG cout << "\nRead source flags of landuse : " << landuse;

	char   oneLine[2540];
	ifstream IF_landuse_sourcesFile;
        string S_landuse_sourcesFileName;
        S_landuse_sourcesFileName = S_prefix + "/code/src/ExportTargets/"+S_InputFile+"/" + "landuse_sources.dat";

        IF_landuse_sourcesFile.open( S_landuse_sourcesFileName.c_str() );
        if ( ! IF_landuse_sourcesFile.good() )
        {
                cout << "Unable to open file: " << S_landuse_sourcesFileName << "\n";
                return -1;
        }

	string S_oneline, S_data;
	stringstream datastream;
	*I_nSources = 0;
        getline(IF_landuse_sourcesFile, S_oneline);
	std::stringstream linestream(S_oneline);

	while ( getline(linestream, S_data, ',') )
		datastream << S_data << " ";
	datastream >> S_data;
	//cout << S_data << " ";
	while ( datastream >> S_data )
	{
		//cout << " " << S_data;
		strcpy(C_source[(*I_nSources)++], S_data.c_str());
	}
	//cout << "\nI_nSources = " << *I_nSources << "\n";

	char tempC[256];
	//stringstream datastream2;
	while ( getline(IF_landuse_sourcesFile, S_oneline) )
	{
		//cout << " S_oneline= " << S_oneline << "\n";
		stringstream linestream2;
		stringstream datastream2;
		linestream2 << S_oneline;
		//while ( getline(linestream2, S_data, ',') )
		while ( linestream2.getline(tempC,256,',') )
		{
			//cout << "%" << tempC << " ";
	                datastream2 << tempC << " ";
		}
		datastream2 >> S_data;
		//cout << " * " << S_data << " ";
		if ( strcmp ( S_data.c_str(), landuse ) == 0 )
		{
			for (int I_nSource = 0; I_nSource < *I_nSources; I_nSource++)
				datastream2 >> I_source[I_nSource];
			//cout << "\n" << landuse;
			//for (int I_nSource = 0; I_nSource < *I_nSources; I_nSource++)
				//cout << "\t" << I_source[I_nSource];
			IF_landuse_sourcesFile.close();
			return 0;
		}
	}

	for (int I_nSource = 0; I_nSource < *I_nSources; I_nSource++)
		I_source[I_nSource] = 0;	
	IF_landuse_sourcesFile.close();
	return 0;
}

int main()
{

	string S_species, S_basin, S_scenario, S_InputFile;

	DEBUG cout << "\n\n";
	DEBUG cout << "\nEnter basin for analysis: "; cin >> S_basin;
	DEBUG cout << "\nEnter nutrient species  : "; cin >> S_species;
	DEBUG cout << "\nEnter scenario to use   : "; cin >> S_scenario;
	DEBUG cout << "\nEnter input file        : "; cin >> S_InputFile;

	char C_NorP[2], C_isN[2], C_isP[2];
	if ( S_species == "TOTN" || S_species == "NO3X" || S_species == "NH3X" )
		strcpy(C_NorP, "N");
	if ( S_species == "TOTP" || S_species == "PO4X" || S_species == "ORGP" )
		strcpy(C_NorP, "P");
	strcpy(C_isN,"N");
	strcpy(C_isP,"P");

	int I_flag = 0;

	char   oneLine[2540];
	string S_oneline;

	string S_l1l2FileName;
	ifstream IF_l1l2File;
	int I_nL2lu, I_nL2lus;
	int I_nL1lu, I_nL1lus;

	char   L1names[MAXLUS][256];	
	char   L2L1name[MAXLUS][2][256];
	int    L2L1nums[MAXLUS];
        char   L1name[MAXLUS][256];
	double L1weightL1[MAXLUS], L1weightL2[MAXLUS];
	double L2weight[MAXLUS];
	int    L2columnFwd[MAXLUS];
	int    L2columnBck[MAXLUS];
	char   S_riverSegment[MAXRIVS][256];
	char   S_landSegment[MAXLANDS][256];
	double D_landSegmentAcres[MAXLANDS][MAXLUS];
	double D_L1acres[MAXLUS] = { };
	double D_L2acres[MAXLUS] = { };
	double D_L1export[MAXLUS] = { };
	double D_L2export[MAXLUS] = { };

	int    I_temp;
	double D_temp;
	string S_temp;
	bool   B_found;

	double D_basinLoad;

	std::fill_n(L2columnFwd, MAXLUS, -9);
	std::fill_n(L2columnBck, MAXLUS, -9);


	S_l1l2FileName = S_prefix + "/code/src/ExportTargets/"+S_InputFile+"/" + "L2-L1-Landuses-" + C_NorP + ".dat";
	IF_l1l2File.open( S_l1l2FileName.c_str() );
	if ( ! IF_l1l2File.good() )
	{
		cout << "Unable to open file: " << S_l1l2FileName << "\n";
		return -1;
	}
	IF_l1l2File.getline(oneLine, 256);

	I_nL2lus = 0;
	I_nL1lus = 0;
	while ( IF_l1l2File.good() )
	{
		IF_l1l2File >> L2L1name[I_nL2lus][0];
		IF_l1l2File >> L2weight[I_nL2lus];
		IF_l1l2File >> L2L1name[I_nL2lus][1];

		B_found = false;
		for (I_nL1lu = 0; I_nL1lu < I_nL1lus; I_nL1lu++)
		{
			//cout << "\n cmp " << L2L1name[I_nL2lus][1] << " " << L1name[I_nL1lu] << " " << strcmp(L2L1name[I_nL2lus][1], L1name[I_nL1lu]);
			if ( strcmp(L2L1name[I_nL2lus][1], L1name[I_nL1lu]) == 0 )
			{
				B_found = true;
				break;
			}
		}
		L2L1nums[I_nL2lus] = I_nL1lu;
		IF_l1l2File >> L1weightL2[I_nL2lus]; //??
		L1weightL1[I_nL1lu] = L1weightL2[I_nL2lus];
		//cout << "\n%" << B_found;
		if  ( B_found == false )
		{
			strcpy(L1name[I_nL1lus],L2L1name[I_nL2lus][1]);
			//cout << "\n$" << L2L1name[I_nL2lus][1] << "--" << L1name[I_nL1lus] << "#";
			I_nL1lus++;
		}

		I_nL2lus ++;
	}
	I_nL1lus = I_nL1lus-1;
	I_nL2lus = I_nL2lus-1;
	IF_l1l2File.close();

	DEBUG cout << "\nNUMBER OF LEVEL~1 LANDUSES: " << I_nL1lus << "\n";
	DEBUG cout <<   "NUMBER OF LEVEL~2 LANDUSES: " << I_nL2lus << "\n";

	DEBUG cout << "\n\n****************************************************";
	DEBUG cout << "\nLEVEL~1 LANDUSES:";
	DEBUG for(I_nL1lu = 0; I_nL1lu < I_nL1lus; I_nL1lu++)
		cout << "\n" << L1name[I_nL1lu];

	DEBUG cout << "\n\n****************************************************";
	DEBUG cout << "\nLEVEL~2 LANDUSES:";
	DEBUG cout << "\nL2lu\tL1lu\tL2wt\tL1wt\tL1#";
	DEBUG for(I_nL2lu = 0; I_nL2lu < I_nL2lus; I_nL2lu++)
	{
		cout << "\n" << L2L1name[I_nL2lu][0] << "\t" << L2L1name[I_nL2lu][1] << "\t" << L2weight[I_nL2lu] << "\t" << L1weightL2[I_nL2lu] << "\t" << L2L1nums[I_nL2lu];
	}

	ifstream IF_basinLoadFile;
        string S_basinLoadFileName;
        S_basinLoadFileName = S_prefix + "/code/src/ExportTargets/"+S_InputFile+"/" + S_basin + "_loads.dat";

	IF_basinLoadFile.open( S_basinLoadFileName.c_str() );
        if ( ! IF_basinLoadFile.good() )
        {
                cout << "Unable to open file: " << S_basinLoadFileName << "\n";
                return -1;
        }
	IF_basinLoadFile.getline(oneLine, 256);

	D_basinLoad     = 0;
	while ( IF_basinLoadFile.good() )
	{
		IF_basinLoadFile >> S_temp;
		IF_basinLoadFile >> D_temp;
		//cout << "\n" << S_temp << "\t" << D_temp << "\t" << S_species;
		if ( S_temp == S_species )
		{
			B_found = true;
			D_basinLoad = D_temp;
		}
		if ( S_temp == "END" )
			break;
	}
	if ( ! B_found )
	{
		cout << "\nSpecies " << S_species << "no found on the file\n";
		return -1;
	}
	IF_basinLoadFile.close();
	DEBUG cout << "\n\n****************************************************";
	DEBUG cout << "\nLOADS FROM " << S_basin << " FOR " << S_species << " = " << D_basinLoad;




	ifstream IF_basinRsegsFile;
        string S_basinRsegsFileName;
        S_basinRsegsFileName = S_prefix + "/code/src/ExportTargets/"+S_InputFile+"/" + S_basin + "_rsegs.dat";

        IF_basinRsegsFile.open( S_basinRsegsFileName.c_str() );
        if ( ! IF_basinRsegsFile.good() )
        {
                cout << "Unable to open file: " << S_basinRsegsFileName << "\n";
                return -1;
        }
        IF_basinRsegsFile.getline(oneLine, 256);

        int I_nRiverSegment, I_nRiverSegments;
	I_nRiverSegments = 0;
	while ( IF_basinRsegsFile.good() )
        {
		IF_basinRsegsFile >> S_riverSegment[I_nRiverSegments];
		//cout << "\t" << S_temp; //S_riverSegment[I_nRiverSegments];
		I_nRiverSegments++;
	}
	I_nRiverSegments = I_nRiverSegments - 1;
	IF_basinRsegsFile.close();

	DEBUG cout << "\n\nNUMBER OF RIVER SEGMENTS " << I_nRiverSegments;




        ifstream IF_basinLsegsFile;
        string S_basinLsegsFileName;
        S_basinLsegsFileName = S_prefix + "/code/src/ExportTargets/"+S_InputFile+"/" + S_basin + "_lsegs.dat";

        IF_basinLsegsFile.open( S_basinLsegsFileName.c_str() );
        if ( ! IF_basinLsegsFile.good() )
        {
                cout << "Unable to open file: " << S_basinLsegsFileName << "\n";
                return -1;
        }
        IF_basinLsegsFile.getline(oneLine, 256);

        int I_nLandSegment, I_nLandSegments;
        I_nLandSegments = 0;
        while ( IF_basinLsegsFile.good() )
        {       
                IF_basinLsegsFile >> S_landSegment[I_nLandSegments];
                //cout << "\t" << S_temp; //S_riverSegment[I_nLandSegments];
                I_nLandSegments++;
        }
        I_nLandSegments = I_nLandSegments - 1;
        IF_basinLsegsFile.close();

	DEBUG cout << "\n\nNUMBER OF LAND SEGMENTS " << I_nLandSegments;





	ifstream IF_acresFile;
        string S_acresFileName;
        S_acresFileName = S_prefix + "/code/src/ExportTargets/"+S_InputFile+"/" + "land_use_" + S_scenario + ".csv";

        IF_acresFile.open( S_acresFileName.c_str() );
        if ( ! IF_acresFile.good() )
        {
                cout << "Unable to open file: " << S_acresFileName << "\n";
                return -1;
        }

	string S_data;
	double D_data;

	getline(IF_acresFile, S_oneline);
	std::stringstream linestream(S_oneline);
	getline(linestream, S_data, ','); // header river-segment
	getline(linestream, S_data, ','); // header land-segment

	I_temp = 0;
	DEBUG cout << "\n\n****************************************************";
	DEBUG cout << "\nlu\tcolumn#~in~land~use~file";
	while ( getline(linestream, S_data, ',') )
	{
		//cout << "\n" << S_data;
		for (int I_nL2lu = 0; I_nL2lu < I_nL2lus; I_nL2lu++)
		{
			if ( strcmp (S_data.c_str(), L2L1name[I_nL2lu][0]) == 0 )
			{
				L2columnFwd[I_nL2lu] = I_temp;
				L2columnBck[I_temp]  = I_nL2lu;
				//DEBUG cout << "\n" << L2L1name[I_nL2lu][0] << " " << L2columnFwd[I_nL2lu];
				DEBUG cout << "\n" << L2L1name[I_nL2lu][0] << " " << L2columnBck[I_temp] + 1;
			}
		}
		I_temp++;
	}

	while ( getline(IF_acresFile, S_oneline) )
        {
                std::stringstream linestream(S_oneline);
                //linestream.str(S_line);
                S_data = "";
                getline(linestream, S_data, ',');
		for ( I_nRiverSegment = 0; I_nRiverSegment < I_nRiverSegments; I_nRiverSegment++)
		{
			if ( strcmp (S_riverSegment[I_nRiverSegment], S_data.c_str() ) == 0 )
			{
				//cout << "\n" << S_data << " ";
				getline(linestream, S_data, ',');
				for ( I_nLandSegment = 0; I_nLandSegment < I_nLandSegments; I_nLandSegment++)
                		{
					if ( strcmp( S_landSegment[I_nLandSegment], S_data.c_str() ) == 0 )
					{
						//cout << S_data << " " ;
						I_temp = 0;
						while (getline(linestream, S_data, ','))
						{
							if ( L2columnBck[I_temp] != -9 )
								D_landSegmentAcres[I_nLandSegment][ L2columnBck[I_temp] ] += atof(S_data.c_str());
							I_temp++;
						}
						break;
					}
				}
				//cout << "\n";
			}
		}
	}

	DEBUG cout << "\n\n****************************************************";
	DEBUG cout << "\nLANDUSE ACRES";
	DEBUG cout << "\nlseg";
	for (int I_nL2lu = 0; I_nL2lu < I_nL2lus; I_nL2lu++)
		cout << "\t" << L2L1name[I_nL2lu][0];
	for ( I_nLandSegment = 0; I_nLandSegment < I_nLandSegments; I_nLandSegment++)
	{
		cout << "\n" << S_landSegment[I_nLandSegment];
		for (int I_nL2lu = 0; I_nL2lu < I_nL2lus; I_nL2lu++)
                {
			cout << "\t" << D_landSegmentAcres[I_nLandSegment][I_nL2lu];
		}
	}










	double D_TotalAcres = 0.0;
	double D_Sigma_weightL1 = 0.0;
	double D_Sigma_wightXareaL1 = 0.0;
	double D_Sigma_wightXareaL2[MAXLUS] = { };
	for (int I_nL1lu = 0; I_nL1lu < I_nL1lus; I_nL1lu++)
        {
		D_Sigma_weightL1 += L1weightL1[I_nL1lu];
	}
	for (int I_nL2lu = 0; I_nL2lu < I_nL2lus; I_nL2lu++)
        {
		for ( I_nLandSegment = 0; I_nLandSegment < I_nLandSegments; I_nLandSegment++)
        	{
			//cout << " " << L2L1nums[I_nL2lu] << " ";
			D_TotalAcres                              += D_landSegmentAcres[I_nLandSegment][I_nL2lu];
			D_L1acres[ L2L1nums[I_nL2lu] ]            += D_landSegmentAcres[I_nLandSegment][I_nL2lu];
			D_L2acres[ I_nL2lu ]                      += D_landSegmentAcres[I_nLandSegment][I_nL2lu];
			D_Sigma_wightXareaL1                      += D_landSegmentAcres[I_nLandSegment][I_nL2lu] * L1weightL2[I_nL2lu];
			D_Sigma_wightXareaL2[ L2L1nums[I_nL2lu] ] += D_landSegmentAcres[I_nLandSegment][I_nL2lu] * L2weight[I_nL2lu];
        	}
	}
	DEBUG cout << "\n\n****************************************************";
	DEBUG cout << "\nLEVEL~1\tACRES\tEXPORT~RATE";
	for (int I_nL1lu = 0; I_nL1lu < I_nL1lus; I_nL1lu++)
        {
		//D_L1export[I_nL1lu] = D_basinLoad * L1weightL1[I_nL1lu] / D_Sigma_wightXareaL1;
		//D_L1export[I_nL1lu] = D_basinLoad * L1weightL1[I_nL1lu] / D_L1acres[I_nL1lu];
		D_L1export[I_nL1lu] = ( D_basinLoad / D_Sigma_weightL1 ) * ( L1weightL1[I_nL1lu] / D_L1acres[I_nL1lu] );
		DEBUG cout << "\n" << L1name[I_nL1lu] << "\t" << D_L1acres[ I_nL1lu ] << "\t" << D_L1export[I_nL1lu];
	}
	DEBUG cout << "\n\n****************************************************";
	DEBUG cout << "\nLEVEL~2\tACRES\tEXPORT~RATE";
	for (int I_nL2lu = 0; I_nL2lu < I_nL2lus; I_nL2lu++)
        {
		//cout << "\n" << L2L1nums[I_nL2lu] << " " << D_L1export[ L2L1nums[I_nL2lu] ] << " " << L2weight[I_nL2lu]; 
		D_L2export[I_nL2lu] = D_L1export[ L2L1nums[I_nL2lu] ] * L2weight[I_nL2lu] / ( D_Sigma_wightXareaL2[ L2L1nums[I_nL2lu] ] / D_L1acres[ L2L1nums[I_nL2lu] ] );
                DEBUG cout << "\n" << L2L1name[I_nL2lu][0] << "\t" << D_L2acres[ I_nL2lu ] << "\t" << D_L2export[I_nL2lu];
	}
	DEBUG cout << "\n";


	char C_source[MAXAPPS][254];// = {"atdep", "fert"};
	int  I_source[MAXAPPS];//      = { 1     , 0     };
	int  I_nSources             = 2;
	int  I_nSource;
	double D_Sigma_appXarea, D_Sigma_area;
	double D_ApplicationRate[MAXLANDS];
	double D_TotalApplication[MAXLANDS];
	double D_Export[MAXLANDS][MAXLUS];
	double D_Intercept[MAXLANDS][MAXLUS];
	double D_sensitivity[MAXAPPS];


	ifstream IF_applicationrate;
	string   S_applicationrate;

	//cout << "\n" << C_source[0] << " " << C_source[1] ;

	for ( I_nL2lu = 0; I_nL2lu < I_nL2lus; I_nL2lu++)
	{
		for ( I_nLandSegment = 0; I_nLandSegment < I_nLandSegments; I_nLandSegment++)
                {
			D_Export[I_nLandSegment][I_nL2lu] = D_L2export[I_nL2lu];
                }


		//Read I_source[MAXAPPS]  for this lu
		I_flag = _Read_I_sources(S_InputFile, L2L1name[I_nL2lu][0], &I_nSources, C_source, I_source);
		for ( I_nSource=0; I_nSource < I_nSources; I_nSource++)
			DEBUG cout << ",\t" << C_source[I_nSource] << " " << I_source[I_nSource];
		if (I_flag != 0 ) return I_flag;

		I_flag = _Read_D_sensitivity(L2L1name[I_nL2lu][0], S_InputFile, S_species, I_nSources, C_source, D_sensitivity);
		if (I_flag != 0 ) return I_flag;

		for ( I_nSource = 0; I_nSource < I_nSources; I_nSource++ )
		{
			if ( I_source[I_nSource] == 1 )
			{
				//TODO: Read Application Rates for SOURCE-LU
				//D_ApplicationRate[1] = 0;//13.03347454;
				//D_ApplicationRate[0] = 0;//31.32992125;
				for ( I_nLandSegment = 0; I_nLandSegment < I_nLandSegments; I_nLandSegment++)
				{
					//../../../output/S_InputFile/NLDc8514HydBeWQb/fert_A10001_hom_1984_2014.csv
					S_applicationrate = S_prefix + "/output/"+S_InputFile+"/NLDc8514HydBeWQb/" + C_source[I_nSource] + "_" + S_landSegment[I_nLandSegment] + "_" + L2L1name[I_nL2lu][0] + "_1984_2014.csv";
					IF_applicationrate.open(S_applicationrate.c_str());
					IF_applicationrate.getline(oneLine, 256); //header
					getline(IF_applicationrate, S_oneline); //dataline
					std::stringstream linestream(S_oneline);
					std::stringstream datastream;
					while ( getline(linestream, S_data, ',') )
						datastream << S_data << " ";
					//cout << "\n" << S_landSegment[I_nLandSegment] ;
					//for (int i=0; i<5; i++)
					//{
						//datastream >> S_data; cout << " " << S_data << " ";
					//}
					//datastream >> D_data; cout << "\n" << D_data << ">" << C_NorP[0] << "-" << C_isN << "<"; if ( C_NorP[0] == 'N' ) { D_ApplicationRate[I_nLandSegment]  = D_data; cout << " " << D_ApplicationRate[I_nLandSegment] << "\n"; }
					datastream >> D_data; if ( C_NorP[0] == 'N' ) D_ApplicationRate[I_nLandSegment]  = D_data;
					datastream >> D_data; if ( C_NorP[0] == 'N' ) D_ApplicationRate[I_nLandSegment] += D_data;
					datastream >> D_data; if ( C_NorP[0] == 'N' ) D_ApplicationRate[I_nLandSegment] += D_data;
					datastream >> D_data; if ( C_NorP[0] == 'P' ) D_ApplicationRate[I_nLandSegment]  = D_data;
					datastream >> D_data; if ( C_NorP[0] == 'P' ) D_ApplicationRate[I_nLandSegment] += D_data;

					IF_applicationrate.close();
				}
				for ( I_nLandSegment = 0; I_nLandSegment < I_nLandSegments; I_nLandSegment++)
                                {
					//rout << S_landSegment[I_nLandSegment] << " " << D_ApplicationRate[I_nLandSegment] << "\n";
				}

				D_Sigma_appXarea = 0;
				D_Sigma_area     = 0;
				for ( I_nLandSegment = 0; I_nLandSegment < I_nLandSegments; I_nLandSegment++)
				{
					D_Sigma_appXarea += D_L2acres[ I_nL2lu ] * D_ApplicationRate[I_nLandSegment];
					D_Sigma_area     += D_L2acres[ I_nL2lu ];
				}

				// Call sensitivity function land segment
				for ( I_nLandSegment = 0; I_nLandSegment < I_nLandSegments; I_nLandSegment++)
                                {
					if ( D_Sigma_area > 0 )
						//D_Export[I_nLandSegment][I_nL2lu] += 0.1000000 * ( D_ApplicationRate[I_nLandSegment] - D_Sigma_appXarea / D_Sigma_area );
						D_Export[I_nLandSegment][I_nL2lu]    += D_sensitivity[I_nSource] * ( D_ApplicationRate[I_nLandSegment] - D_Sigma_appXarea / D_Sigma_area );
						D_Intercept[I_nLandSegment][I_nL2lu] += D_sensitivity[I_nSource] * ( D_ApplicationRate[I_nLandSegment] );
                                }
			}
		}
	}


	DEBUG cout << "\n\n****************************************************";
        DEBUG cout << "\nLANDUSE INTERCEPTS:";
        DEBUG cout << "\nlseg";
        for ( I_nL2lu = 0; I_nL2lu < I_nL2lus; I_nL2lu++)
                DEBUG cout << "\t" << L2L1name[I_nL2lu][0];
        for ( I_nLandSegment = 0; I_nLandSegment < I_nLandSegments; I_nLandSegment++)
        {
                DEBUG cout << "\n" << S_landSegment[I_nLandSegment];
                for ( I_nL2lu = 0; I_nL2lu < I_nL2lus; I_nL2lu++)
                {
			D_Intercept[I_nLandSegment][I_nL2lu] = D_Export[I_nLandSegment][I_nL2lu] - D_Intercept[I_nLandSegment][I_nL2lu];
                        DEBUG cout << "\t" << D_Intercept[I_nLandSegment][I_nL2lu];
                }
        }



	DEBUG cout << "\n\n****************************************************";
	DEBUG cout << "\nLANDUSE EXPORT RATES:";
	DEBUG cout << "\nlseg";
	for ( I_nL2lu = 0; I_nL2lu < I_nL2lus; I_nL2lu++)
		DEBUG cout << "\t" << L2L1name[I_nL2lu][0];
	for ( I_nLandSegment = 0; I_nLandSegment < I_nLandSegments; I_nLandSegment++)
	{
		DEBUG cout << "\n" << S_landSegment[I_nLandSegment];
		for ( I_nL2lu = 0; I_nL2lu < I_nL2lus; I_nL2lu++)
		{
			DEBUG cout << "\t" << D_Export[I_nLandSegment][I_nL2lu];
		}
	}

	


	cout << "\n";
	return 0;
}

