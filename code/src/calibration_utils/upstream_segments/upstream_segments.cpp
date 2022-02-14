#include <iostream>
#include <fstream>
#include <string>

#define MAXRIVS 3000

using namespace std;



void findup(string riv, string *S_Rivers,int *I_RivID, int* I_DownID, int I_NRivs, int i)
{
	int j;
        for(j=0; j<I_NRivs; j++)
        {
                if ( I_RivID[I_DownID[j]] == I_RivID[i] )
                {
			cout << "\n" << riv << "," << S_Rivers[j];
                        findup(riv, S_Rivers, I_RivID, I_DownID, I_NRivs, j);
                }
        }
}



int main()
{
	string S_Rivers[MAXRIVS];
	int    I_RivID[MAXRIVS];
	int    I_DownID[MAXRIVS];

	string S_RivFile;

	cin >> S_RivFile;

	string S_RivFileName = "../../../../config/seglists/" + S_RivFile + ".riv";

	ifstream IF_RivFile;
	IF_RivFile.open( S_RivFileName.c_str() );

	string S_temp;
	IF_RivFile >> S_temp;
	IF_RivFile >> S_temp;
	IF_RivFile >> S_temp;
	IF_RivFile >> S_temp;

	int I_NRivs = 0;
	while ( IF_RivFile )
	{
		IF_RivFile >> S_temp;

		if ( S_temp == ")" ) break;

		S_Rivers[I_NRivs] = S_temp;
		I_RivID[I_NRivs]  = atoi(S_temp.substr(4,4).c_str());
		//cout << S_temp << "\t" << I_RivID[I_NRivs] << "\n";
		I_NRivs++;

	}

	for(int i=0; i<I_NRivs; i++) I_DownID[i] = -9;
	//cout << "\n";
	for(int i=0; i<I_NRivs; i++)
	{
		for(int j=0; j<I_NRivs; j++)
		{
			//cout << "\n" << S_Rivers[i] << " " << S_Rivers[j];
			if ( atoi(S_Rivers[i].substr(9,4).c_str()) == atoi(S_Rivers[j].substr(4,4).c_str()) )
			{
				//cout << " X ";
				I_DownID[i] = j;
				break;
			}
		}
	}

	cout << "RiverSegment,UpStreamSeg";
	for(int i=0; i<I_NRivs; i++)
	{
		if ( atoi(S_Rivers[i].substr(9,4).c_str()) != 3 )
			cout << "\n" << S_Rivers[i] << "," << S_Rivers[i];
		findup(S_Rivers[i], S_Rivers, I_RivID, I_DownID, I_NRivs, i);
	}


	cout << "\n";
	return 0;
}

