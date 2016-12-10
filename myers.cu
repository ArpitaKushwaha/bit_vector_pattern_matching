#include <thrust/copy.h>
#include <thrust/fill.h>
#include <thrust/sequence.h>
#include <stdio.h>
#include "bitVector.cpp"
#include <omp.h>
#include <string.h>
#include <math.h>
#include <bitset>
#include <climits>
#include <vector>
#include <ctype.h>
//#include "../common/common.h"
#include<sys/time.h>
#include <cuda_runtime.h>
#include <iostream>
#include <thrust/device_vector.h> 
#include <thrust/host_vector.h>
using namespace std;
#define cudaCheckErrors(msg) \
    do { \
        cudaError_t __err = cudaGetLastError(); \
        if (__err != cudaSuccess) { \
            fprintf(stderr, "Fatal error: %s (%s at %s:%d)\n", \
                msg, cudaGetErrorString(__err), \
                __FILE__, __LINE__); \
            fprintf(stderr, "*** FAILED - ABORTING\n"); \
            exit(1); \
        } \
    } while (0)

class Sequence{
	public:
	    char * name;
	    char * seq;
	    long size;
	    void readSequence(const char * file);
	    
};
	

void Sequence::readSequence(const char * file)
{
	FILE *f1 = fopen(file, "r" );
       	fpos_t position;
	
	char s1[100];    
       	fgets(s1,100,f1);
	name = (char *)malloc(strlen(s1));
        memset(name, 0, strlen(s1));

	for( int i = 0; i < strlen(s1); i++)
	{	
		name[i] = s1[i+1];
	}	

       	fgetpos (f1, &position);
       	int x = ftell(f1);
       	fsetpos (f1, &position);
       	fseek(f1,SEEK_SET , SEEK_END);
       	int y = ftell(f1);
       	size = y-x;
       	seq = new char[size];
       	fsetpos(f1, &position);
       	fread( seq, size, 1, f1 );
	size = strlen(seq);
	for( int i=0 ; i < strlen(seq); ++i )
		seq[i] = toupper( seq[i] ) ;

       	fclose( f1 );
}

__global__ void preprocessing( char * d_characterSet, char * d_vec, char * d_query, int len, int charLength )
{
	int i;
	int j = 0;
	int len1 = len + 1;
        for( i = len-1; i >= 0; i--)
        {
                if( d_characterSet[threadIdx.x] == d_query[i] )
                {
			d_vec[threadIdx.x*len1 + j ] = '1';
                }
                else
                {
                        d_vec[threadIdx.x*len1 + j ] = '0';
                }
		j++;
        }
	d_vec[threadIdx.x*len1+j] = '\0';
}

char*  lookup(char dev_seq, char* char_set, int len)
{
	char * str_ptr;
	str_ptr = (char*)malloc(sizeof(char)*len);
	switch(dev_seq){
	
		case 'A':{
			for (int i = 0;i<len;i++)
				str_ptr[i] = char_set[0*len+i];
			break;
			}
		case 'G':{
			  for (int i = 0;i<len;i++)
                                str_ptr[i] = char_set[1*len+i];
			break;
			}
		case 'C':{
			 for (int i = 0;i<len;i++)
                                str_ptr[i] = char_set[2*len+i];
			break;
			}	
		case 'T':{
			  for (int i = 0;i<len;i++)
                                str_ptr[i] = char_set[3*len+i];
			break;
			}	
		default:
			 memset(str_ptr, '0', sizeof(char)*len);

	}
	return str_ptr;
}

char* binary_add( char opd1[], char opd2[], int len)
{
	int len1 = len + 1;
	char* result;;  // To store the sum bits
	result = (char*)malloc(sizeof(char)*len1); 
 	memset(result, '0', sizeof(char)*len1);
	bool carry = false;  // Initialize carry
	std::string sum = "";
	int charToIntOne;
	int charToIntTwo;
    	for (int i = len-1; i >= 0 ; i--)
    	{
		charToIntOne = opd1[i] - '0';
        	charToIntTwo = opd2[i] - '0';
 
        	if (carry == true && (charToIntOne + charToIntTwo) >= 1)
        	{
            		sum += "0";
        	}
        	else if (carry == true && (charToIntOne + charToIntTwo) == 0)
        	{
            		sum += "1";
            		carry = false;
        	}
        	else
        	{
            		if ((charToIntOne + charToIntTwo) > 1)
            		{
                		sum += "0";
                		carry = true;
            		}
            		else if ((charToIntOne + charToIntTwo) == 1)
            		{
                		sum += "1";
                		carry = false;
            		}
            		else
            		{
                		sum += "0";
                		carry = false;
            		}
        	}
    	}
	int k; 
    	// if overflow, then add a leading 1
    	if (carry == true){
        	sum += "1";
		k = sum.size()-2;
	}
	else{
		k = sum.size()- 1;
	}
	char res[len1];
	//cout<<sum.size()<<"    "<<sum<<endl;
	for (int j = 0; j < len; j++)
    	{
		  res[j] = sum.at(k);
		  k--;
    	}
	res[len +1] = '\0';
	//memset(result,res[len2],sizeof(char)*len2);
	strcpy(result,res);
	//cout<<result<<endl;
    	return result;
}

char* leftShift( char opd[], int len)
{
	char * res;
	res = (char*) malloc((len)*sizeof(char));
	memset(res,'0',len);
	for (int i = 0; i < len; i++) 
	{
		opd[i] = opd[i+1];
		if( i == len - 1)
			opd[i] = '0';	
	}
	strcpy(res,opd);
	return res;
}
__global__ void ORoperation( char * dev_opd1, char * dev_opd2, char * dev_res, int len) 
{
		dev_res[threadIdx.x] = dev_opd1[threadIdx.x] | dev_opd2[threadIdx.x];
	//	printf("%c OR %c = %c\n",dev_opd1[threadIdx.x],dev_opd2[threadIdx.x],dev_res[threadIdx.x]);
}

__global__ void ANDoperation( char* dev_opd1, char* dev_opd2, char* dev_AND, int len)
{
	//	printf("id : %d \n", threadIdx.x);    
		dev_AND[threadIdx.x] = dev_opd1[threadIdx.x] & dev_opd2[threadIdx.x];
	//	printf("%c AND %c = %c\n",dev_opd1[threadIdx.x],dev_opd2[threadIdx.x],dev_AND[threadIdx.x]);
}

__global__ void  XORoperation( char* dev_opd1, char* dev_opd2, char* dev_XOR, int len)
{
		int a = dev_opd1[threadIdx.x] - '0';
		int b = dev_opd2[threadIdx.x] - '0';
		int res = a ^ b;
		dev_XOR[threadIdx.x] = res + '0';
}

__global__ void NOToperation( char* dev_opd, char* dev_NOT, int len)
{
		if( dev_opd[threadIdx.x] == '0' )
			dev_NOT[threadIdx.x] = '1';
		else
			dev_NOT[threadIdx.x] = '0';
}

int main()
{
	//computing Time
	struct timeval t1, t2;

	gettimeofday(&t1, 0);

	int mismatch = 800;
	//Reading the sequences from FASTA files
        Sequence database;
	const char * file = "1000_sequence.txt";
	database.readSequence(file);
	cout<<"Database Details"<<endl;
        cout<<"name : "<<database.name<<endl;
        cout<<"size : "<<database.size<<endl;
       //cout<<"sequence"<<endl<<database.seq;
	
	Sequence query[2];


	const char * file1 = "pattern.txt";
	query[0].readSequence(file1);
        cout<<"Query 0 Details"<<endl;
        cout<<"name : "<<query[0].name<<endl;
        cout<<"size : "<<query[0].size<<endl;
       //cout<<"sequence"<<endl<<query[0].seq;
	
	/*const char * file2 = "query1.txt";
        query[1].readSequence(file2);
        cout<<"Query 1 Details"<<endl;
        cout<<"name : "<<query[1].name<<endl;
        cout<<"size : "<<query[1].size<<endl;
        cout<<"sequence"<<endl<<query[1].seq;*/

	char characterSet[] = "AGCT";
		
	for( int i = 0; i < 1; i++)
	{	//int i = 0;
		char * d_characterSet;
		char * d_query;
		int num_cores = strlen(characterSet);
		int len = query[i].size - 1;
                int len1 = query[i].size;
		char h_vec[num_cores][len1];
		char * d_vec;
		cudaMalloc((char**)&d_vec, num_cores*len1*sizeof(char));
		
		cudaMalloc((char**)&d_query, len* sizeof(char));
		cudaMalloc((char**)&d_characterSet, strlen(characterSet)*sizeof(char));

		cudaMemcpy( d_query, query[i].seq, len, cudaMemcpyHostToDevice);
		cudaMemcpy( d_characterSet, characterSet, strlen(characterSet), cudaMemcpyHostToDevice );
		
		dim3 block(num_cores);
		preprocessing <<< 1, num_cores >>> (d_characterSet, d_vec, d_query, len, strlen(characterSet));
		//cudaDeviceSynchronize();
		//cudaCheckErrors("error");
		cudaMemcpy( h_vec, d_vec, num_cores*len1, cudaMemcpyDeviceToHost );
		/*for ( int j = 0;j<num_cores;j++)
		{
			for(int k = 0;k<len1;k++)
			{
				cout<<h_vec[j][k];
			}
			cout<<endl;
		}*/
		
		char VN[len1];
		memset(VN,'0',sizeof(char)*len);
		char VP[len1];
		memset(VP,'1',sizeof(char)*len);
		char X[len];
		
		char D0[len];
                memset(D0,'0',sizeof(char)*len);
		char HN[len];
                memset(HN,'0',sizeof(char)*len);
		char HP[len];
                memset(HP,'0',sizeof(char)*len);
		char temp[len];
		memset(temp,'0',sizeof(char)*len);
		int score = len;
//		begin = clock();
		//cout<<"VP: "<<VP<<endl;
		//cout<<"VN: "<<VN<<endl;
		cout<<"The max score: "<<score<<endl;
		for( int k = 0; k < database.size - 1; k++ )
		{
			char* ptr;

			ptr = lookup( database.seq[k], (char*)h_vec, len1 );
			char Deq[len1];
			strncpy(Deq, ptr, (len1)*sizeof(char));
			//cout<<"******************Reading Text["<<k<<"]*****"<<database.seq[k]<<"****************************************"<<endl;
			//cout<<"Deq: "<<Deq<<endl;
				
			//OR operation on device 
			char* dev_opd1;
			char* dev_opd2;
			char* dev_OR;
			cudaMalloc((char**)&dev_opd1,len1*sizeof(char));  
			cudaMalloc((char**)&dev_opd2, len1*sizeof(char));
			cudaMalloc((char**)&dev_OR, len*sizeof(char));
			cudaMemcpy( dev_opd1, Deq, len1, cudaMemcpyHostToDevice );
			cudaMemcpy( dev_opd2, VN, len1, cudaMemcpyHostToDevice );
			
			dim3 block1(len);
			ORoperation <<< 1, len >>> ( dev_opd1, dev_opd2, dev_OR, len);
			//cudaDeviceSynchronize();
                	cudaCheckErrors("error");
			cudaMemcpy(X, dev_OR, len, cudaMemcpyDeviceToHost );
			/*cout<<"X is  ";
			for( int x=0;x<len;x++)
				cout<<X[x];
			cout<<endl;*/
			
			char* dev_AND;
			char host_AND[len];
			cudaMalloc((char**)&dev_AND, len*sizeof(char));
			cudaMemcpy( dev_opd2, VP, len, cudaMemcpyHostToDevice);
			ANDoperation <<<1,len>>> ( dev_OR, dev_opd2, dev_AND, len);
			cudaMemcpy(host_AND, dev_AND, len, cudaMemcpyDeviceToHost);
			/*cout<<"AND of X and VP: ";
			for( int x=0;x<len;x++)
                                cout<<host_AND[x];
                        cout<<endl;*/
			
			char* sum_ptr;
			sum_ptr = binary_add( host_AND, VP, len);
			char host_SUM[len1];
                        strncpy(host_SUM, sum_ptr,(len1)*sizeof(char));
			//cout<<"Sum of host_AND & VP : "<<host_SUM<<endl;

			char host_XOR[len];
			char* dev_XOR;
			//memset(host_XOR, '0', sizeof(char) * len);
			cudaMalloc((char**)&dev_XOR, len*sizeof(char));
			//cudaMalloc((char**)&dev_opd1, len*sizeof(char));
			//cudaMemcpy( dev_XOR, host_XOR, len, cudaMemcpyHostToDevice);
			cudaMemcpy( dev_opd1, host_SUM, len1, cudaMemcpyHostToDevice);
			XORoperation <<<1,len>>> (dev_opd1, dev_opd2, dev_XOR, len);
			//cudaDeviceSynchronize();
                	cudaCheckErrors("error");
			cudaMemcpy(host_XOR, dev_XOR, len, cudaMemcpyDeviceToHost);
			/*cout<<" XOR: ";
			for( int x=0;x<len;x++)
                                cout<<host_XOR[x];
                        cout<<endl;*/

			cudaMemcpy( dev_opd2, X, len, cudaMemcpyHostToDevice );
			//cudaMemcpy( dev_OR, D0, len, cudaMemcpyHostToDevice );
			ORoperation <<< 1,len >>> ( dev_XOR, dev_opd2, dev_OR, cudaMemcpyHostToDevice);
			cudaMemcpy( D0, dev_OR, len, cudaMemcpyDeviceToHost);
			/*cout<<" D0: ";
			for( int x=0;x<len;x++)
                                cout<<D0[x];
                        cout<<endl;*/

			cudaMalloc((char**)&dev_opd1, len*sizeof(char));
			cudaMemcpy( dev_opd1, VP, len, cudaMemcpyHostToDevice);
			ANDoperation <<<1,len>>> (dev_opd1, dev_OR, dev_AND, len);
			//cudaDeviceSynchronize();
                	cudaCheckErrors("error");
			cudaMemcpy( HN, dev_AND, len, cudaMemcpyDeviceToHost);
			/*cout<<" HN: ";
			for( int x=0;x<len;x++)
                                cout<<HN[x];
                        cout<<endl;*/
			
			cudaMemcpy( dev_opd2, D0, len, cudaMemcpyHostToDevice );
			ORoperation <<<1,len>>> (dev_opd1, dev_opd2, dev_OR, len);
			//cudaDeviceSynchronize();
                	cudaCheckErrors("error");
			cudaMemcpy(temp, dev_OR, len, cudaMemcpyDeviceToHost);
			/*cout<<"OR of D0 and VP: ";  
			for( int x=0;x<len;x++)
                                cout<<temp[x];
                        cout<<endl;*/
		
			char * dev_NOT;
			cudaMalloc((char**)&dev_NOT, len*sizeof(char));
			NOToperation <<<1,len>>> ( dev_OR, dev_NOT, len);
			//cudaDeviceSynchronize();
                	cudaCheckErrors("error");
			cudaMemcpy(temp, dev_NOT, len, cudaMemcpyDeviceToHost);
			/*cout<<" not: ";
			for( int x=0;x<len;x++)
                                cout<<temp[x];
                        cout<<endl;*/

			cudaMemcpy(dev_opd1, VN, len, cudaMemcpyHostToDevice);
			ORoperation <<<1,len>>> ( dev_opd1, dev_NOT, dev_OR, len);
			//cudaDeviceSynchronize();
                	cudaCheckErrors("error");
			cudaMemcpy(HP, dev_OR, len, cudaMemcpyDeviceToHost);
			/*cout<<" HP: ";
			for( int x=0;x<len;x++)
                                cout<<HP[x];
                        cout<<endl;*/

			//score check
			char h_arr[len];
			int ind;
			for( ind = 0; ind < len; ind++ )
			{
				if(ind == 0){
					h_arr[ind] = '1';
				}
				else{
					h_arr[ind] = '0';
				}
			}
			char tmp1[len];
			char tmp2[len];
			/*cout<<"should be 100: ";
			 for( int x=0;x<len;x++)
                                cout<<h_arr[x];
                        cout<<endl;*/	
			cudaMemcpy(dev_opd1, h_arr, len, cudaMemcpyHostToDevice);
                        cudaMemcpy(dev_opd2, HP, len, cudaMemcpyHostToDevice);
                        ANDoperation <<<1,len>>> (dev_opd1, dev_opd2, dev_AND, len);
			cudaMemcpy(tmp1, dev_AND, len, cudaMemcpyDeviceToHost);
			//cudaDeviceSynchronize();
                	cudaCheckErrors("error");
                        
                        /*cout<<"HP AND 100:  ";
			for( int x=0;x<len;x++)
                                cout<<tmp1[x];
                        cout<<endl;*/

                        cudaMemcpy(dev_opd2, HN, len, cudaMemcpyHostToDevice);
                        ANDoperation <<<1,len>>> (dev_opd1, dev_opd2, dev_AND, len);
			//cudaDeviceSynchronize();
                	cudaCheckErrors("error");
                        cudaMemcpy(tmp2, dev_AND, len, cudaMemcpyDeviceToHost);
                        /*cout<<"HN AND 100:  ";
			for( int x=0;x<len;x++)
                                cout<<tmp2[x];
                        cout<<endl;*/
			
			
			bool res1 = false;
			bool res2 = false;
			
			for( int y = 0; y < len; y++ )
			{
				if(tmp1[y] != '0')
				{	
					res1 = true;
					break;
				}
			}			
	
			for( int i = 0; i < len; i++ )
                        {
                                if(tmp2[i] != '0')
                                {
                                        res2 = true;
                                        break;
                                }
                        }
			if( res1 == true)		
				score = score + 1;
			else if( res2 == true)
				score = score - 1;
							
			//cout<<"score: "<<score<<endl;

			if( score <= mismatch){
				//cout<<"appox. match at position: "<<k<<" score: "<<score<<" text character  " <<database.seq[k]<<endl;
			}
			//VN and VP for next column
			char * shft;
                        shft =  leftShift( HP, len);
                        strncpy(X,shft,len*sizeof(char));
                        /*cout<<"new X: ";
                        for( int x=0;x<len;x++)
                                cout<<X[x];
                        cout<<endl;*/

			cudaMemcpy(dev_opd1, X, len, cudaMemcpyHostToDevice);
			cudaMemcpy(dev_opd2, D0, len, cudaMemcpyHostToDevice);
			ANDoperation <<<1,len>>> (dev_opd1, dev_opd2, dev_AND, len);
			//cudaDeviceSynchronize();
                	cudaCheckErrors("error");
			cudaMemcpy(VN, dev_AND, len, cudaMemcpyDeviceToHost);
			/*cout<<"new VN: ";
			for( int x=0;x<len;x++)
                                cout<<VN[x];
                        cout<<endl;*/

			ORoperation <<<1,len>>> (dev_opd1, dev_opd2, dev_OR, len);
			//cudaDeviceSynchronize();
                	cudaCheckErrors("error");
                        cudaMemcpy(temp, dev_OR, len, cudaMemcpyDeviceToHost);
                        /*cout<<"OR of X & D0: ";
			for( int x=0;x<len;x++)
                                cout<<temp[x];
                        cout<<endl;*/

			NOToperation <<<1,len>>> ( dev_OR, dev_NOT, len);
			//cudaDeviceSynchronize();
                	cudaCheckErrors("error");
                        cudaMemcpy(temp, dev_NOT, len, cudaMemcpyDeviceToHost);
                        /*cout<<" not: ";
			for( int x=0;x<len;x++)
                                cout<<temp[x];
                        cout<<endl;*/

			shft =  leftShift( HN, len);
			char shftHP[len];
                        strncpy(shftHP,shft,len*sizeof(char));
                        //cout<<"shift left of HP: "<<shftHP<<endl;
		
			cudaMemcpy(dev_opd1, shftHP, len, cudaMemcpyHostToDevice);
                        cudaMemcpy(dev_opd2, temp, len, cudaMemcpyHostToDevice);
                        ORoperation <<<1,len>>> (dev_opd1, dev_opd2, dev_OR, len);
			//cudaDeviceSynchronize();
                	cudaCheckErrors("error");
                        cudaMemcpy(VP, dev_OR, len, cudaMemcpyDeviceToHost);
                        /*cout<<"new VP: ";
			for( int x=0;x<len;x++)
                                cout<<VP[x];
                        cout<<endl;*/		
		}	
	
	}		
	
	gettimeofday(&t2, 0);

	double time = (1000000.0*(t2.tv_sec-t1.tv_sec) + t2.tv_usec-t1.tv_usec)/1000000.0;

	printf("Time to generate:  %3.1f ms \n", time);
//	end = clock();
//	time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
 //       printf("Time to generate:  %3.1f \n", time_spent);
	return 0;
}
