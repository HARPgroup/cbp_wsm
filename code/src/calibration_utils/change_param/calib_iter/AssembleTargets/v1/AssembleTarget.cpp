#include <iostream>
#include <fstream>

#define MAXVARS 15
#define MAXROWS 367

using namespace std;

int main(int argc, char* argv[]){

	//char LU[] = "nhy";
	char LU[3];
	char inFileNames[MAXVARS][200];
	ifstream inFile[MAXVARS];

	char landSeg[MAXROWS][7];
	double data[MAXROWS][MAXVARS];

	char inFilePrefix[] = "/bluefish/archive/modeling/p600/output/pltgen/summary/p532cal_062211/";
	char pltnames[][5] = {"SNH4", "INH4", "DNH4", "BNH4", "SNO3", "INO3", "BNO3", "SLON", "ILON", "DLON", "BLON", "SRON", "IRON", "DRON", "BRON"};

	strcpy(LU, argv[1]); cout << "Processing LU = " << LU << "\n";

	for (int i=0; i<MAXVARS; i++){
		//nhy_BLON_sum.csv
		sprintf(inFileNames[i],"%s%s_%s_sum.csv",inFilePrefix,LU,pltnames[i]);
		//cout << inFileNames[i] << "\n";
		inFile[i].open(inFileNames[i]);
	}

	char tempString[20];
	for (int i=0; i<MAXVARS; i++){
		for(int j=0; j<MAXROWS; j++){
			//inFile[i] >> tempString;
			//std::stringstream tempStringStream(tempString);

			if(i == 0){
				inFile[i].getline(tempString, 256, ',');
				inFile[i].getline(tempString, 256, ',');
				inFile[i].getline(tempString, 256, ','); strcpy(landSeg[j], tempString); //cout << landSeg[j] << "\n"; 
				inFile[i].getline(tempString, 256, ',');
				inFile[i].getline(tempString, 256, ',');
				//inFile[i].getline(tempString, 256);      data[j][i] = atof(tempString);
				inFile[i] >> data[j][i]; //cout << data[j][i] << "\n"; 
			}
			else{
				inFile[i].getline(tempString, 256, ',');
                                inFile[i].getline(tempString, 256, ',');
                                inFile[i].getline(tempString, 256, ',');
                                inFile[i].getline(tempString, 256, ',');
                                inFile[i].getline(tempString, 256, ',');
                                //inFile[i].getline(tempString, 256);      data[j][i] = atof(tempString);
				inFile[i] >> data[j][i];
			}
		}
	}


	char outFileName[200];
        ofstream outFile;

	sprintf(outFileName,"%s_Assembled.csv",LU);
	outFile.open(outFileName);
	//cout << outFileName;
 
	outFile << "LandSegs";
	for (int i=0; i<MAXVARS; i++){
		outFile << ",";
		for (int j=0; j<4; j++){
			outFile << pltnames[i][j];
		}
	}
	outFile << "\n";
	for (int i=0; i<MAXROWS; i++){
		//cout << "$"<<landSeg[i]<<"%" << "\t";
		for(int j=0; j<7; j++)
			outFile << landSeg[i][j];
                for(int j=0; j<MAXVARS; j++){
			outFile << "," << data[i][j];
		}
		outFile << "\n";
	}
	outFile.close();
	//cout << inFilePrefix << "\t" << pltnames[0] << "\n";

}
