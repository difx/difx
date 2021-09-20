
#include <vector>
#include <stdio.h>
#include <string>
#include <iostream>
#include <sstream>
#include <fstream>
#include <mark5access/mark5_stream.h>
#include <math.h>
#include <cstring>
using namespace std;

int countbits(unsigned char v);
static int findfirstframe(const unsigned char*, int, int);
int binConv(int n);
std::vector<int> extract_time_code();
long long filesize(char * filename);

int main(int argc, char* argv[]) {
   
    std::fstream inputFileStream;
    std::vector<char*> inputfiles;


    // Mark 4 format stuff, we assume MKIV-1024-16-2
    long long framedata_bits = 19840;
    long long ntracks = 64;
    long long frameheader_bits = 160;
    long long offset = 0; // first header offset in bytes
    long long readlength = 0; 
    long frameheader_bytes = frameheader_bits/8;
    frameheader_bytes = ntracks * frameheader_bytes;
    frameheader_bits = ntracks * frameheader_bits;


    // Read in inputfiles  
    for (int jj = 1; jj < argc; ++jj) {
        inputfiles.push_back(argv[jj]); 
    }

      

    // loop over input files
    int total_files = inputfiles.size();
    for (int filenum=0; filenum<total_files; ++filenum) {
        // Create a copy of the input file named [INPUT FILE NAME].mod 
        std::ifstream  src(inputfiles[filenum], std::ios::binary);
        const char* ext = ".mod";
        char* newname = inputfiles[filenum];
  
        std::strcat(newname,ext);
        std::ofstream  dst(newname,   std::ios::binary);
        dst << src.rdbuf();

        //std::cout << "inputfiles[0] = " << inputfiles[0] << std::endl;
        //std::cout << "inputfiles[1] = " << inputfiles[1] << std::endl;
        //exit(0);
 
       
        // Open [INPUT FILE NAME].mod for writing, origal input file will remian unchanged
        // all changes will be recorded in the copy.
        fstream outputFileStream;
        outputFileStream.open(newname,fstream::in | fstream::out | fstream::binary);

         
        // Frame data is interalced  
        long long frameheader_bits_di = frameheader_bits/ntracks;
        inputFileStream.close();
        inputFileStream.open(inputfiles[filenum],ios::in | ios::binary);   
        // To find the first headers we read at least 2 frames worth of data
        // each frame is data+header * number of tracks
        readlength = 2 *((framedata_bits + frameheader_bits_di) * ntracks);
        // convert to bytes
        readlength = readlength/8;
        
        // Declare buffer with readlength
        unsigned char buffer[readlength];

        // Find the first frame of the data file, this involves reading 2 framelenths worth of data and finding a pattern
        // that fits the header         
        inputFileStream.read((char*)(&buffer[0]),readlength);
        offset = findfirstframe(buffer, readlength, ntracks);
        offset = offset - 512;
    
        // Determine filesize and the number of frames contained in the file
        long long filesize_bytes = filesize(inputfiles[filenum]);
        long long framesize = ((framedata_bits + frameheader_bits_di) * ntracks);
        long long framesize_bytes = framesize/8;

        double numframe_dbl = (filesize_bytes-offset)/(framesize_bytes);         
        long long numframes = floor(numframe_dbl);
        for (long long frm=0; frm<numframes; ++frm) {
           
            // De-interlace full data frame (ignoring pre offset data for now)            
            long long read_pos = offset+((frm*(framesize_bytes)));
            inputFileStream.seekg(read_pos);
            readlength = 0;
            readlength = framesize;
            readlength = readlength/8;
            // Deinterlaced frame will be in bits
            long long deinterlaced_framelength = (readlength*8)/ntracks;
            unsigned char frame_buffer[readlength];     
            unsigned char deinterlaced_frame[ntracks][deinterlaced_framelength];
            inputFileStream.read((char*)(&frame_buffer[0]),readlength);
            long long  gg = 0;
            // Convert frame_buffer to bits.  In memory C++ stores this as bytes with 7 0s and the last bit being actual data eg: 0000000[1 or 0]
            // the below loop looks at the the bits in each byte and stores them as the last bit in a byte.  This wastes space but makes the code
            // sytax much more understandable
            unsigned char frame_buffer_bits[readlength*8];
            long long frame_buffer_size = sizeof(frame_buffer)*8;
            for (long long ii=0;ii<frame_buffer_size;++ii) {
                frame_buffer_bits[ii] = ((1 << (ii % 8)) & (frame_buffer[ii/8])) >> (ii % 8);
            }
        
                   
            // Organize bits into a 2-D array 
            for (long long yy=0;yy<deinterlaced_framelength;++yy) {
                for (int xx=0;xx<ntracks;++xx) {
                    deinterlaced_frame[xx][yy] = frame_buffer_bits[gg];
                    gg = gg+1;
                }
            }
        
        
            unsigned char fh_bits[frameheader_bits_di];
            unsigned char fh_bits_mod[frameheader_bits_di];
            unsigned char fh_bits_test[frameheader_bits_di];  
            // Make a copy of the data frame that we'll use for writing
            unsigned char deinterlaced_frame_copy[ntracks][deinterlaced_framelength];
            for (long long yy=0;yy<deinterlaced_framelength;++yy) {
                for (int xx=0;xx<ntracks;++xx) {
                    deinterlaced_frame_copy[xx][yy] = deinterlaced_frame[xx][yy];
                }
            }

           // Create a copy of the 2-D Switch tracks 6 <> 10  and 8 <> 12
           // changing limit of y will allow exclusiong of header 
           for (long yy=160;yy<deinterlaced_framelength;++yy) {
                deinterlaced_frame_copy[4][yy] = deinterlaced_frame[8][yy];
                deinterlaced_frame_copy[8][yy] = deinterlaced_frame[4][yy];
                deinterlaced_frame_copy[6][yy] = deinterlaced_frame[10][yy];
                deinterlaced_frame_copy[10][yy] = deinterlaced_frame[6][yy];
                deinterlaced_frame_copy[36][yy] = deinterlaced_frame[40][yy];
                deinterlaced_frame_copy[40][yy] = deinterlaced_frame[36][yy];
                deinterlaced_frame_copy[38][yy] = deinterlaced_frame[42][yy];
                deinterlaced_frame_copy[42][yy] = deinterlaced_frame[38][yy];
           }
                        
            unsigned char frame_buffer_bits_mod[readlength*8];
     
            // Re-Interlace frame
            long long kk = 0;  
            for (long long yy=0;yy<deinterlaced_framelength;++yy) {
                 for (int xx=0;xx<ntracks;++xx) { 
                     frame_buffer_bits_mod[kk] = deinterlaced_frame_copy[xx][yy];
                     kk = kk+1;
                 }
            }

           // Combined 8 elements from from frame_buffer_bits_mod into individual bytes stored in frame_buffer_bytes_mod
           long long writelength = readlength;
           unsigned char frame_buffer_bytes_mod[writelength];
           long long pp = 0;

           for (long long ii = 0; ii<writelength; ++ii) { 
               frame_buffer_bytes_mod[ii] = 0;
               for (int cc=0; cc<8; ++cc) {
                   unsigned char shifted_bit = frame_buffer_bits_mod[pp]<<cc; 
                   frame_buffer_bytes_mod[ii] = frame_buffer_bytes_mod[ii] | shifted_bit; 
                   pp = pp+1;
               }
           }
           long long frame_buffer_size_bits = sizeof(frame_buffer)*8;
           unsigned char frame_buffer_bits_test[frame_buffer_size_bits];
           for (long long ii=0;ii<frame_buffer_size_bits;++ii) {
               frame_buffer_bits_test[ii] = ((1 << (ii % 8)) & (frame_buffer_bytes_mod[ii/8])) >> (ii % 8); 
           }
           unsigned char deinterlaced_frame_test[ntracks][frameheader_bits_di];
           long long ggg = 0;
           for (long long yy=0;yy<frameheader_bits_di;++yy) {
                for (int xx=0;xx<ntracks;++xx) {
                    deinterlaced_frame_test[xx][yy] = frame_buffer_bits_test[ggg];
                    ggg = ggg+1;
                }
           }
                                                                    
            // Overwrite modified frames into output data file

            outputFileStream.seekg(offset+(frm*framesize_bytes));
            outputFileStream.write((char *)&frame_buffer_bytes_mod[0],writelength);            
            // Work is done at this point the rest of the code is for testing purposes
            
            //std::cout << "inputfile[1] = " << inputfiles[1] << std::endl;
            
            if ((1 < inputfiles.size()) && (strcmp(inputfiles[1],"test"))) {
              for (int xx=0;xx<ntracks;++xx) {
                  for (long long yy=0;yy<frameheader_bits_di;++yy) {
                      fh_bits[yy] = deinterlaced_frame[xx][yy];     
                      fh_bits_mod[yy] = deinterlaced_frame_copy[xx][yy]; 
                      fh_bits_test[yy] = deinterlaced_frame_test[xx][yy];
                  }
                
                 // Find time code for each track and frame to see if it makes sense 
                 long long cc_1 = 0;
                 std::vector<int> time_code_digits_1;
                 std::string tc_digit_1;
                 std::stringstream tc_digit_ss_1;
                 long long tc_digit_int_1 = 0;
                 if (0 == xx) {
                     std::cout << "Track num = " << xx << " timecode binary:      ";
                     for (int yy=96;yy<148;++yy) {
                           
                           cc_1 = cc_1+1;
                           tc_digit_int_1 = fh_bits[yy] & 0x1;
                           std::cout << tc_digit_int_1;
                           tc_digit_ss_1 << tc_digit_int_1;
              
                           tc_digit_1.append(tc_digit_ss_1.str());
                           tc_digit_ss_1.str("");
                           if((cc_1%4 == 0) && (cc_1>0)) { 
                               std::stringstream tc_digit_full_ss_1(tc_digit_1);
                               int tc_digit_full_int_1 = 0;
                               tc_digit_full_ss_1 >> tc_digit_full_int_1;
                               time_code_digits_1.push_back(tc_digit_full_int_1);
                               tc_digit_1.clear();
                           }
              
                      }
  
                      // test
                      int tc_digit_mod = 0;
                      std::cout << "Track num = " << xx << " timecode binary mod:  ";
                      for (int yy=96;yy<148;++yy) {
                          tc_digit_mod = fh_bits_mod[yy] & 0x1;
                          std::cout << tc_digit_mod;
                      }
                      std::cout << std::endl;
  
                      int tc_digit_test = 0;
                      std::cout << "Track num = " << xx << " timecode binary test: ";
                      for (int yy=96;yy<148;++yy) {
                          tc_digit_test = fh_bits_test[yy] & 0x1;
                          std::cout << tc_digit_test;
                      }
                      std::cout << std::endl;
  
                      int tc_decimal_1 = 0;
                      
                      std::cout << "Frame number = " << frm << " Timecode  = ";
                      int time_code_digits_1_size = time_code_digits_1.size();
                      for (int ii=0; ii<time_code_digits_1_size; ++ii) {
                          tc_decimal_1 = binConv(time_code_digits_1[ii]);
                          std::cout << tc_decimal_1;
                      }
                      std::cout << std::endl;
                  } 
             
              }
            }
        }   
    }    
     
              
    
        // re-interlace and then write frame to a new file 
    

    return 0;
}


int countbits(unsigned char v)
{
        unsigned int c; // c accumulates the total bits set in v

        for (c = 0; v; ++c)
        {
                v &= v - 1; // clear the least significant bit set
        }

        return c;
}

// Finds first data frame
/* Look for the first occurrence (lowest offset >= 0) of the following pattern:
 *  *
 *   * 32*tracks bits set at offset bytes
 *    * 32*tracks bits set at offset+2500*tracks bytes
 *     * 1*tracks bits unset at offset+2499*tracks bytes
 *      *
 *       * return offset;
 *        */
static int findfirstframe(const unsigned char* data, int bytes, int tracks)
{
	int offset;
	int wrong = 0;
	int i, a, b;
	int cbits[256];
	unsigned char c;


	if(bytes < 2600*tracks)
	{
		return -1;
	}

	for(c = 0;; ++c)
	{
		cbits[c] = countbits(c);
		//std::cout << "counbits(c) = " << countbits(c) << std::endl;
		if(c == 255)
		{
			break;
		}
	}

	bytes -= 2600*tracks;

	b = tracks*2500;
	a = b - tracks/8;

	for(i = 0; i < 4*tracks; ++i)
	{
		if(cbits[(unsigned char)(data[i])] < 6)
		{
			++wrong;
		}
		if(cbits[(unsigned char)(data[i+b])] < 6)
		{
			++wrong;
		}
	}
	for(i = 0; i < tracks/8; ++i)
	{
		if(cbits[(unsigned char)(data[i+a])] > 2)
		{
			++wrong;
		}
	}

	for(offset = 0; offset < bytes; ++offset)
	{
		if(wrong == 0)
		{
			return offset;
		}
		if(cbits[(unsigned char)(data[offset])] < 6)
		{
			--wrong;
		}
		if(cbits[(unsigned char)(data[offset+4*tracks])] < 6)
		{
			++wrong;
		}
		if(cbits[(unsigned char)(data[offset+b])] < 6)
		{
			--wrong;
		}
		if(cbits[(unsigned char)(data[offset+b+4*tracks])] < 6)
		{
			++wrong;
		}
		if(cbits[(unsigned char)(data[offset+a])] > 2)
		{
			--wrong;
		}
		if(cbits[(unsigned char)(data[offset+a+tracks/8])] > 2)
		{
			++wrong;
		}
	}

	return -1;
}

// Convert Binary string to decimal number
int binConv(int n)
{
    int num = n;
    int dec_value = 0;
 
//Initializing base value to 1, i.e 2^0
    int base = 1;
    int temp = num;
    while (temp) {
        int last_digit = temp % 10;
        temp = temp / 10;
        dec_value += last_digit * base;                                  
        base = base * 2;
    }                                                
    return dec_value;
}


long long filesize(char * filename) {
   ifstream in_file(filename, ios::binary);
   in_file.seekg(0, ios::end);
   long long file_size = in_file.tellg();
   return file_size;

}
