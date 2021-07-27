
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

    // MPI Stuff

    //MPI_Init(&argc, &argv);
    //MPI_Comm world;
    //int myID = -1;
    //double restartseconds = 0.0;
    //world = MPI_COMM_WORLD;
    //MPI_Comm_rank(world, &myID);
    // Mark 4 format stuff, we assume MKIV-1024-16-2
    long long framedata_bits = 19840;
    long long ntracks = 64;
    long long frameheader_bits = 160;
    long long offset = 0; // first header offset in bytes
    long long readlength = 0; 
    // test
//    long a = 10;
//    long b = 20;
//    long long x = 0;
//    x = gcd(a,b);
//    std::cout << "x = " << x << std::endl;
    //if (argc > 2) {
//	std::cout << "usage: bitOrderFT (exp).input" << std::endl;
//        return 0;  
//    }

    for (int jj = 1; jj < argc; ++jj) {
        inputfiles.push_back(argv[jj]); 
    }


    inputFileStream.open(inputfiles[0],std::ios::in | std::ios::binary);
    // To find the first headers we read at least 2 frames worth of data
    // each frame is data+header * number of tracks
    readlength = 2 *((framedata_bits + frameheader_bits) * ntracks);
    // convert to bytes
    readlength = readlength/8;
    //std::cout << "readlength = " << readlength << std::endl;
 
    //declare buffer with readlength
    unsigned char buffer[readlength]; 

    inputFileStream.read((char*)(&buffer[0]),readlength);     
    offset = findfirstframe(buffer, readlength, ntracks);   
    offset = offset - 512; 
    //std::cout << "offset = " << offset << std::endl;
    long frameheader_bytes = frameheader_bits/8;
    // Read the header there are 64 interlaced headers 
    frameheader_bytes = 64 * frameheader_bytes;

    unsigned char header[frameheader_bytes];

   
    inputFileStream.seekg(offset);
    inputFileStream.read((char*)(&header[0]),frameheader_bytes); 
    //std::cout << "offset = " << offset << std::endl;
    // Expand header into bits, *note each "bit" will be a 0 or 1 character, this can't be wrtitten to a file  
    // 64 headers
    frameheader_bits = 64*frameheader_bits;
    unsigned char header_bits[frameheader_bits];
    int header_size_bits = sizeof(header)*8;
    for (long ii=0;ii<header_size_bits;++ii) {
        header_bits[ii] = ((1 << (ii % 8)) & (header[ii/8])) >> (ii % 8);
    }
    long deinterlaced_header_bits = frameheader_bits/64;
    unsigned char deinterlaced_headers[64][deinterlaced_header_bits];
    // Headers need to be de-interlaced
    long long aa = 0;
    for (long long yy=0;yy<deinterlaced_header_bits;++yy) {
        for (int xx=0;xx<64;++xx) { 
            deinterlaced_headers[xx][yy] = header_bits[aa]; 
            aa = aa+1;
        }
    }
    int cc = 0; 
    std::vector<int> time_code_digits;
    std::string tc_digit;
    std::stringstream tc_digit_ss;
    int tc_digit_int = 0;
    for (int yy=96;yy<148;++yy) {
        //printf("%d",deinterlaced_headers[0][yy] & 0x1);
        cc = cc+1;
        tc_digit_int = deinterlaced_headers[0][yy] & 0x1;

        //std::cout << "tc_digit_int = " << tc_digit_int << std::endl;
        tc_digit_ss << tc_digit_int;
        //std::cout << "tc_digit_ss = " << tc_digit_ss.str() << std::endl;
        tc_digit.append(tc_digit_ss.str());
        tc_digit_ss.str("");
       
        if ((cc%4 == 0) && (cc>0)) {
           //std::cout << "tc_digit = " << tc_digit << std::endl;
           std::stringstream tc_digit_full_ss(tc_digit);
           int tc_digit_full_int = 0;
           tc_digit_full_ss >> tc_digit_full_int;
           //std::cout << "tc_digit_full_int = " << tc_digit_full_int << std::endl;
           time_code_digits.push_back(tc_digit_full_int);
           tc_digit.clear();
        } 
           

    }
    //std::cout << std::endl;

    // Display time code
    //int tc_decimal = 0;
    //int time_code_digits_size = time_code_digits.size();  
    //for (long long ii=0; ii<time_code_digits_size; ++ii) {
    //    tc_decimal = binConv(time_code_digits[ii]);   
       // std::cout << tc_decimal;
    //}
    //std::cout << std::endl;
    


    // loop over input files
    int total_files = inputfiles.size();
    for (int filenum=0; filenum<total_files; ++filenum) {
        // copy file 
        std::ifstream  src(inputfiles[filenum], std::ios::binary);
        const char* ext = ".mod";
        char* newname = inputfiles[filenum];
        std::strcat(newname,ext);
        //std::cout << "newname = " << newname << std::endl;

        std::ofstream  dst(newname,   std::ios::binary);

        dst << src.rdbuf();
       
        // open outputfile for writing
        fstream outputFileStream;
        outputFileStream.open(newname,fstream::in | fstream::out | fstream::binary);

         
        // find first frame offset in file
        // loop over frames
               
    
    
        long long frameheader_bits_di = frameheader_bits/64;

    
          
        inputFileStream.close();
        inputFileStream.open(inputfiles[filenum],ios::in | ios::binary);   
        //To find the first headers we read at least 2 frames worth of data
        // each frame is data+header * number of tracks
        readlength = 2 *((framedata_bits + frameheader_bits_di) * ntracks);
        // convert to bytes
        readlength = readlength/8;
        //std::cout << "readlength = " << readlength << std::endl;
        
        //declare buffer with readlength
        unsigned char buffer_1[readlength];
        
        inputFileStream.read((char*)(&buffer_1[0]),readlength);
        offset = findfirstframe(buffer_1, readlength, ntracks);
        offset = offset - 512;
        //std::cout << "offset = " << offset << std::endl;                                         
    
        // Determine filesize and the number of frames contained in the file
        long long filesize_bytes = filesize(inputfiles[filenum]);
        //std::cout << "filesize bytes = " << filesize_bytes << std::endl;        
        //std::cout << "framedata_bits = " << framedata_bits << std::endl;
        //std::cout << "frameheader_bits = " << frameheader_bits_di << std::endl;
        //std::cout << "ntracks = " << ntracks << std::endl;
        long long framesize = ((framedata_bits + frameheader_bits_di) * ntracks);
        //std::cout << "framesize = " << framesize << std::endl;
        long long framesize_bytes = framesize/8;
        //std::cout << "framesize_bytes = " << framesize_bytes << std::endl;

        double numframe_dbl = (filesize_bytes-offset)/(framesize_bytes);         
        long long numframes = floor(numframe_dbl);
        //std::cout << "numframes = " << numframes << std::endl;         
        //exit(0);
        for (long long frm=0; frm<numframes; ++frm) {
           
            // de-interlace full data frame (ignoring pre offset data for now)            
            //std::cout << "framesize_bytes = " << framesize_bytes << std::endl;
            long long read_pos = offset+((frm*(framesize_bytes)));
            //offset = offset-512;
            inputFileStream.seekg(read_pos);
            readlength = 0;
            readlength = framesize;
            readlength = readlength/8;
            //std::cout << "readlength = " << readlength << " read_pos = " << read_pos << std::endl; 
            // deiterlaced frame will be in bits
            long long deinterlaced_framelength = (readlength*8)/ntracks;
            unsigned char frame_buffer[readlength];     
            unsigned char deinterlaced_frame[ntracks][deinterlaced_framelength];
            inputFileStream.read((char*)(&frame_buffer[0]),readlength);
            long long  gg = 0;
            // convert frame_buffer to bits
            unsigned char frame_buffer_bits[readlength*8];
            long long frame_buffer_size = sizeof(frame_buffer)*8;
            for (long long ii=0;ii<frame_buffer_size;++ii) {
                frame_buffer_bits[ii] = ((1 << (ii % 8)) & (frame_buffer[ii/8])) >> (ii % 8);
            }
        
                  
        
            //std::cout << "deinterlaced_framelength = " << deinterlaced_framelength << std::endl;
            for (long long yy=0;yy<deinterlaced_framelength;++yy) {
                for (int xx=0;xx<ntracks;++xx) {
                    //std::cout << frame_buffer[gg] << std::endl;
                    deinterlaced_frame[xx][yy] = frame_buffer_bits[gg];
                    gg = gg+1;
                }
            }
        
            //std::cout << "gg = " << gg << std::endl;
            //std::cout << "Size of frame_buffer = " << sizeof(frame_buffer) << std::endl;
        
            //now check header timecodes to see if they make sense
            unsigned char fh_bits[frameheader_bits_di];
            unsigned char fh_bits_mod[frameheader_bits_di];
            unsigned char fh_bits_test[frameheader_bits_di];  
            // make a copy of the data frame that we'll use for writing
            unsigned char deinterlaced_frame_copy[ntracks][deinterlaced_framelength];
            for (long long yy=0;yy<deinterlaced_framelength;++yy) {
                for (int xx=0;xx<ntracks;++xx) {
                    deinterlaced_frame_copy[xx][yy] = deinterlaced_frame[xx][yy];
                }
            }
            //for (int bb=0; bb<160; ++bb) {   
            //   int blah1 = deinterlaced_frame[0][bb] & 0x1;
            //   int blah2 = deinterlaced_frame_copy[0][bb] & 0x1;
            //   std::cout << bb << "   " << blah1 << "    " << blah2 << std::endl;
            //} 
           //exit(0);
                    //                                                                    
 
           // std::copy(&deinterlaced_frame[0][0], &deinterlaced_frame[0][0]+ntracks*deinterlaced_framelength, &deinterlaced_frame_copy[0][0]); 
           // for (int cc=0; cc<ntracks;++cc) {
           //   for (int bb=0;bb<deinterlaced_framelength;++bb) {
           //      if (deinterlaced_frame_copy[cc][bb] != deinterlaced_frame[cc][bb]) {
           //          std::cout << "frame and frame copy differ!" << std::endl;
           //      }
           //   }
           // }
           // exit(0); 


            // switch tracks 6 <> 10  and 8 <> 12
           // changing limit of y will allow exclusiong of header 
           for (long yy=160;yy<deinterlaced_framelength;++yy) {
                deinterlaced_frame_copy[6][yy] = deinterlaced_frame[10][yy];
                deinterlaced_frame_copy[10][yy] = deinterlaced_frame[6][yy];
                deinterlaced_frame_copy[8][yy] = deinterlaced_frame[12][yy];
                deinterlaced_frame_copy[12][yy] = deinterlaced_frame[8][yy];
           }
                        
            //int test_1 = deinterlaced_frame_copy[6][0] & 0x1;
            //int test_2 = deinterlaced_frame[10][0] & 0x1;
            //std::cout << "bit in copy arrays: " << test_1 << std::endl;
            //std::cout << "bit in original array: " << test_2 << std::endl;              
            //int test_3 = deinterlaced_frame_copy[8][0] & 0x1;
            //int test_4 = deinterlaced_frame[12][0] & 0x1;
            //std::cout << "bit in copy arrays: " << test_1 << std::endl;
            //std::cout << "bit in original array: " << test_2 << std::endl;
            //int test_5 = deinterlaced_frame_copy[12][0] & 0x1;
            //int test_6 = deinterlaced_frame[8][0] & 0x1;
            //std::cout << "bit in copy arrays: " << test_1 << std::endl;
            //std::cout << "bit in original array: " << test_2 << std::endl;
            //std::exit(0);
            //
            unsigned char frame_buffer_bits_mod[readlength*8];
     
            // Re-Interlace frame
            long long kk = 0;  
            for (long long yy=0;yy<deinterlaced_framelength;++yy) {
                 for (int xx=0;xx<ntracks;++xx) { 
                     frame_buffer_bits_mod[kk] = deinterlaced_frame_copy[xx][yy];
                     kk = kk+1;
                 }
            }
            long long rlbyt = readlength; 
            //for (long long cc=0; cc<rlbyt; ++cc) {
            //    if (frame_buffer_bits_mod[cc] != frame_buffer_bits[cc]) {
            //       std::cout << "frame_buffer_bits_mod and frame_buffer_bits differ at " << cc << std::endl;
            //    } 
            //}
            //exit(0); 
           //for (long yy=0;yy<1000;++yy) {

          //    int one = frame_buffer_bits_mod[yy] & 0x1;
	  //    int two = frame_buffer_bits[yy] & 0x1;     
          //    std::cout << one << "   " << two << std::endl; 
          // }
           //exit(0);




           // convert bits back into bytes for writing, format is in raw bits but c++ works with bytes
           long long writelength = readlength;
           unsigned char frame_buffer_bytes_mod[writelength];
           //std::cout << "writelength = " << writelength << std::endl;
           //std::cout << "readlength  = " << readlength << std::endl; 
           long long pp = 0;

           std::cout << std::endl;
           for (long long ii = 0; ii<writelength; ++ii) { 
               frame_buffer_bytes_mod[ii] = 0;
               for (int cc=0; cc<8; ++cc) {
                   //std::cout << "cc = " << cc << std::endl;
                   

                   if (((pp >= 6144) && (pp <=9472))) { 
                          long long two = frame_buffer_bytes_mod[ii]; 
                          //std::cout << cc << " " << ii << " " << two<< std::endl;
                      if (pp % 64 == 0) {
                          int blurg = frame_buffer_bits_mod[pp];  
                          //std::cout << blurg;
                          //long long one = frame_buffer_bytes_mod[ii-1];
                      }
                   }
                   unsigned char shifted_bit = frame_buffer_bits_mod[pp]<<cc; 
                   frame_buffer_bytes_mod[ii] = frame_buffer_bytes_mod[ii] | shifted_bit; 
                   pp = pp+1;
               }
               if (((pp >= 6144) && (pp <=9472))) {
                   if (pp % 64 == 0) {
                       long long blag = frame_buffer_bytes_mod[ii];
                       //std::cout << std::endl << "frame_buffer_bytes_mod[ii] = " << blag << std::endl;
                   }
               }
               //exit(0);
           }
           std::cout << std::endl;
           //exit(0);
           // Start here!
           // test 
           long long frame_buffer_size_bits = sizeof(frame_buffer)*8;
           unsigned char frame_buffer_bits_test[frame_buffer_size_bits];
           //std::cout << "frame_buffer_size_bits = " << frame_buffer_size_bits << std::endl;
           //std::cout << "frame_buffer_bits_test = ";
           for (long long ii=0;ii<frame_buffer_size_bits;++ii) {
               frame_buffer_bits_test[ii] = ((1 << (ii % 8)) & (frame_buffer_bytes_mod[ii/8])) >> (ii % 8);
               if (((ii >= 6144) && (ii <=9472))) {
                   if (ii % 64 == 0) {
                        int blurg = frame_buffer_bits_test[ii];  
                        //std::cout << blurg;
                   }
               }

           }
           //std::cout << std::endl;
           //exit(0);
           unsigned char deinterlaced_frame_test[ntracks][frameheader_bits_di];
           long long ggg = 0;
           for (long long yy=0;yy<frameheader_bits_di;++yy) {
                for (int xx=0;xx<ntracks;++xx) {
                    //std::cout << "gg = " << gg << std::endl;
                    deinterlaced_frame_test[xx][yy] = frame_buffer_bits_test[ggg];
                    ggg = ggg+1;
                }
           }
           //std::cout << "ggg = " << ggg << std::endl;
           //exit(0);

           //std::cout << "Size of frame_buffer_bits_test = " << sizeof(frame_buffer_bits_test) << std::endl;
           //std::cout << "Size of frame_buffer_bits = " << sizeof(frame_buffer_bits) << std::endl;
           //std::cout << "frame_buff_size_bits = " << frame_buffer_size_bits << std::endl;
           //for (int xxx=0;xxx<frame_buffer_size_bits;++xxx){
           //    int blah = frame_buffer_bits_test[xxx];
           //    int blah1 = frame_buffer_bits_mod[xxx];
           //    int blah2 = frame_buffer_bits[xxx];
           //    std::cout << blah << "    " << blah1 << "     " << blah2 <<std::endl;
           //}
          
 
          //std::cout << std::endl;
            //exit(0);
                                                                     
            // Overwrite modified frames into output data file

            //std::cout << "framesize_bytes = " << framesize_bytes << std::endl;
            //std::cout << "writelength = " << writelength << std::endl;
            outputFileStream.seekg(offset+(frm*framesize_bytes));
            outputFileStream.write((char *)&frame_buffer_bytes_mod[0],writelength);            
            //if (frm ==5) {
            //   exit(0);
            //}           

         
            for (int xx=0;xx<ntracks;++xx) {
                for (long long yy=0;yy<frameheader_bits_di;++yy) {
                    long long oo = (xx*frameheader_bits_di)+yy;
                    //std::cout << "oo = " << oo << std::endl; 
                    fh_bits[yy] = deinterlaced_frame[xx][yy];     
                    fh_bits_mod[yy] = deinterlaced_frame_copy[xx][yy]; 
                    fh_bits_test[yy] = deinterlaced_frame_test[xx][yy];
                    if (xx == 0){    
                        int blah = fh_bits[yy];   
                        int blah2 = deinterlaced_frame_copy[xx][yy];  /*fh_bits_mod[yy] & 0x1;*/
		        //std::cout << xx << "  " << yy << "   " << blah << "    " << blah2 << std::endl;
                    } 
                }
              
               //if (xx == 0) {
               //    std::cout << std::endl;
               //}
               // find time code for each track and frame to see if it makes sense 
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
                         //std::cout << "tc_digit_int_1 = " << tc_digit_int_1 << std::endl;
                         tc_digit_ss_1 << tc_digit_int_1;
            
                         //std::cout << "tc_digit_ss_1.str() = " << tc_digit_ss_1.str() << std::endl;
                         tc_digit_1.append(tc_digit_ss_1.str());
                         tc_digit_ss_1.str("");
                         if((cc_1%4 == 0) && (cc_1>0)) { 
                             std::stringstream tc_digit_full_ss_1(tc_digit_1);
                             int tc_digit_full_int_1 = 0;
                             tc_digit_full_ss_1 >> tc_digit_full_int_1;
                             //std::cout << "tc_digit_full_int_1 = " << tc_digit_full_int_1 << std::endl;
                             time_code_digits_1.push_back(tc_digit_full_int_1);
                             tc_digit_1.clear();
                         }
            
                    }
                    std::cout << std::endl;

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


        std::cout << "bytes = " << bytes << std::endl;
        long x = 2600*tracks;
        std::cout << "2600*tracks = " << x << std::endl;
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
