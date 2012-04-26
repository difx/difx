/***************************************************************************
 *   Copyright (C) 2006-2011 by Walter Brisken                             *
 *   Copyright (C) 2012 by Chris Phillips                                  *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: m5d.c 3588 2011-07-30 23:23:54Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
// $LastChangedRevision: 3588 $
// $Author: WalterBrisken $
// $LastChangedDate: 2011-07-31 09:23:54 +1000 (Sun, 31 Jul 2011) $
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "m5time";
const char author[]  = "Chris Phillips";
const char version[] = "0.1";
const char verdate[] = "20120330";

static void usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("A Mark5 dtime ecoder.  Can decode VLBA, Mark3/4, Mark5B and VDIF"
		"formats using the\nmark5access library.\n\n");
	printf("Usage : %s <file> <dataformat>\n\n", pgm);
	printf("  <file> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: "
		"<FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n");
	printf("    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n\n");
}

int cal2mjd(int day, int month, int year);

int main(int argc, char **argv) {
  int day, month, year, mjdnow, vlbanow, basenow, mjd, millisec, hours, minutes;
  time_t currenttime;
  struct tm *tp;
  struct mark5_stream *ms;

  if(argc !=3 )	{
    usage(argv[0]);
    return EXIT_FAILURE;
  }

  ms = new_mark5_stream_absorb(new_mark5_stream_file(argv[1], 0),
			       new_mark5_format_generic_from_string(argv[2]));

  if(!ms) {
    fprintf(stderr, "Error: problem opening or decoding %s\n", argv[1]);
    return EXIT_FAILURE;
  }

  currenttime = time(NULL);
  tp = localtime(&currenttime);
  month = tp->tm_mon+1;
  day   = tp->tm_mday;
  year  = tp->tm_year+1900;
 
  mjdnow = cal2mjd(day, month, year);

  vlbanow = mjdnow % 1000;
  basenow = mjdnow - vlbanow;

  mjd = ms->mjd + basenow;

  if (mjd+1>mjdnow) mjd -= 1000;

  printf("Assuming MJD = %d/", mjd);

  millisec = ms->sec*1000 + ms->ns/1e6;

  hours = millisec / (60*60*1000);
  millisec -= hours*60*60*1000;
  
  minutes = millisec / (60*1000);
  millisec -= minutes*60*1000;

  printf("%02d:%02d:%05.2f\n", hours, minutes, millisec/1000.0);

  delete_mark5_stream(ms);

  return EXIT_SUCCESS;
}

int cal2mjd(int day, int month, int year) {

  int m, y, c, x1, x2, x3;

  if (month <= 2) {
    m = month+9;
    y = year-1;
  } else {
    m = month-3;
    y = year;
  }

  c = y/100;
  y = y % 100;


  x1 = (146097.0*c/4.0);
  x2 = (1461.0*y/4.0);
  x3 = ((153.0*m+2.0)/5.0);
  return (x1+x2+x3+day-678882);
}








