/***************************************************************************
 *   Copyright (C) 2009 by Jan Wagner                                      *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mark5access.h>

#define MAX_LIST_LEN (512*1024)

int main(int argc, char* argv[])
{
    struct mark5_stream *ms;
    float **unpacked;
    int rate, channels, fanout, bitreso, format;
    char *longlist, *longlist_alloc;
    char *filename;
    int combinations_tested = 0;
    const int rates[] = { 16, 32, 64, 128, 256, 512, 1024, 2048 }; // only the most common ones...

    if (argc <= 1) {
        fprintf(stderr, 
               "\n  Usage: m5findformats <filename>\n\n"
               "  Tries to open the file using MKIV/VLBA/Mark5B formats with\n"
               "  all combinations of fan-out, data rate, channel count and\n"
               "  Successful combinations are reported.\n\n");
        return EXIT_FAILURE;
    }

    filename = argv[1];

    longlist_alloc = malloc(MAX_LIST_LEN);
    longlist = longlist_alloc;
    longlist[0] = '\0';

    unpacked = (float**)malloc(64 * sizeof(float*));
    for (channels = 0; channels < 64; channels++) {
        unpacked[channels] = (float*)malloc(1024*sizeof(float));
    }

    fclose(stderr);
    for (format = 0; format <= 3; format++) {
        for (fanout = 1; fanout <= 8; fanout *= 2) {
            for (rate = 0; rate < sizeof(rates)/sizeof(int); rate++) {
                for (channels = 1; channels <= 64; channels *= 2) {
                    for (bitreso = 1; bitreso <= 4; bitreso *= 2) {
                         int decode_rc = -1;
                         char formatstring[256]; // <FORMAT>-<Mbps>-<nchan>-<nbit>
                         switch (format) {
                             case 0:
                                 snprintf(formatstring, 256, "MKIV1_%d-%d-%d-%d", fanout, rates[rate], channels, bitreso);
                                 break;
                             case 1:
                                 snprintf(formatstring, 256, "VLBA1_%d-%d-%d-%d", fanout, rates[rate], channels, bitreso);
				 break;
                             case 2:
                                 snprintf(formatstring, 256, "Mark5B-%d-%d-%d", rates[rate], channels, bitreso);
                                 if (fanout != 1) continue;
                                 break;
                             case 3:
                                 snprintf(formatstring, 256, "VDIF_64-%d-%d-%d", rates[rate], channels, bitreso);
                                 if (fanout != 1) continue;
                                 break;
                             default:
                                 snprintf(formatstring, 256, "unknown format=%d", format);
				 break;
                         }
                         ++combinations_tested;
                         ms = new_mark5_stream(
                            new_mark5_stream_file(filename, 0),
                            new_mark5_format_generic_from_string(formatstring)
                         );
                         if (ms) {
                             decode_rc = mark5_stream_decode(ms, 1024LL, unpacked);
                         }
	                 if (ms && (decode_rc >= 0)) {
                             if (format == 3) {
                                 snprintf(formatstring, 256, "VDIF_%d-%.0f-%d-%d", ms->databytes, ms->Mbps, ms->nchan, ms->nbit);
                             }
                             fprintf(stdout, "OK: fmt = %s , decoded = %d/1024\n", formatstring, decode_rc);
                             longlist = strcat(longlist, formatstring);
                             longlist = strcat(longlist, "  ");
                             //mark5_stream_print(ms);
                         } else {
                             //fprintf(stdout, "FAIL: fmt = %s , rc = %d\n", formatstring, decode_rc);
                         }
                     }
                }
            }
        }
   }

   fprintf(stdout, "Tested %d combinations.\n", combinations_tested);
   if (strlen(longlist) > 0) {
       fprintf(stdout, "Potentially correct format strings:\n%s\n\n", longlist);
       return EXIT_SUCCESS;
   } else {
       fprintf(stdout, "None of the tested format strings worked on file %s!\n\n", filename);
       return EXIT_FAILURE;
   }
}
