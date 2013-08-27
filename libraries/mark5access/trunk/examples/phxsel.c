/***************************************************************************
 *   Copyright (C) 2008 by Walter Brisken                                  *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include "config.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int usage()
{
	printf("phxsel <mode> infile outfile\n");
	printf("<mode> is:\n");
	printf("  s    -- select\n");
	printf("  e    -- expand\n");

	return 0;
}

int main(int argc, char **argv)
{
	char mode;	/* 's'elect or 'e'xpand */
	int in, out;
	unsigned char inbuf[1024], outbuf[2048];
	int n, m, i, j;

	if(argc != 4)
	{
		return usage();
	}

	mode = argv[1][0];
	if(mode != 's' && mode != 'e')
	{
		return usage();
	}

	in = open64(argv[2], O_RDONLY | O_LARGEFILE);
	if(in <= 0)
	{
		printf("Cannot open %s for read\n", argv[2]);
		return 0;
	}
	out = open64(argv[3], O_WRONLY | O_CREAT | O_LARGEFILE, S_IRUSR | S_IWUSR);
	if(!out)
	{
		close(in);
		printf("Cannot open %s for write\n", argv[3]);
		return 0;
	}

	for(j = 0;; j++)
	{
		n = read(in, inbuf, 1024);
		if(n == 0)
		{
			break;
		}

		if(mode == 's')
		{
			m = n/2;
			for(i = 0; i < n; i+=2)
			{
				outbuf[i/2] = (inbuf[i] & 0x0F) |
					    ( (inbuf[i+1] & 0x0F) << 4);
			}
		}
		else
		{
			m = n*2;
			for(i = 0; i < n; i++)
			{
				outbuf[2*i]   = (inbuf[i] & 0x0F) * 0x11;
				outbuf[2*i+1] = ((inbuf[i] & 0xF0) >> 4) * 0x11;
			}
		}
		write(out, outbuf, m);
	}

	printf("%d kiB input converted\n", j);

	close(in);
	close(out);

	return 0;
}
