/***************************************************************************
 *   Copyright (C) 2014-2019 by Jan Wagner                                 *
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
//
// mk6copy [-s] <Mark6 scan name> <destination>
//
// An example program for the mark6sg library. Copies scatter-gather data
// into an output file.
//
//============================================================================

#include <mark6sg.h>

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h> // clock_gettime(), TODO need alternative for OS X

static char* m_root = MARK6_SG_ROOT_PATTERN;

void usage()
{
	printf("\nmk6copy [-s] <Mark6 scan name> <destination>\n\n"
		"Copies the Mark6 scatter-gather data of a given scan via the\n"
		"libmark6sg library (not FUSE) into a single output file. A scan\n"
		"name of test.vdif for example collects the scatter-gather file\n"
		"fragments '%s/test.vdif'.\n\n"
		"Options:\n"
		"    -s  show progress\n\n"
		"Environment variables:\n"
		"    MARK6_ROOT       search path for Mark 6 data, with wildcards\n"
		"    MARK6_META_ROOT  path containing Mark 6 metadata\n\n",
		m_root
	);
}

int main(int argc, char** argv)
{
	int         fdi, fdo;
	char        *inname;
	char        *outname;
	off_t       nremain = 0, ncopied = 0, ntotal = 0;
	ssize_t     blocksize;
	char*       buf;
	struct stat st;
	int         optarg = 1;

	int         do_progress = 0;
	double      T_mk6read = 0.0;
	double      T_write = 0.0;
	off_t       Nops = 0, Nreport = 0;

	struct timespec t0_rd, t1_rd;
	struct timespec t0_wr, t1_wr;

	/* Arguments */
	if (getenv("MARK6_ROOT") != NULL)
	{
		m_root = strdup(getenv("MARK6_ROOT"));
		mark6_sg_set_rootpattern(m_root);
	}
	if (getenv("MARK6_META_ROOT") != NULL)
	{
		mark6_sg_set_metapath(getenv("MARK6_META_ROOT"));
	}
	while ((optarg < argc) && (argv[optarg][0] == '-'))
	{
		if (strcmp(argv[optarg], "-s") == 0)
		{
			do_progress = 1;
		}
		else
		{
			usage();
			return -1;
		}
		optarg++;
	}
	if ((argc - optarg) != 2)
	{
		usage();
		return -1;
	}
	inname = argv[optarg];
	outname = argv[optarg+1];

	/* Open files */
	fdo = open(outname, O_WRONLY|O_CREAT|O_TRUNC, 0644);
	if (fdo < 0)
	{
		perror(outname);
		return -1;
	}

	if (do_progress) { fprintf(stderr, "Opening Mark6 SG file (can take several seconds)...\n"); }
	fflush(stderr);
	fdi = mark6_sg_open(inname, O_RDONLY);
	if (fdi < 0)
	{
		perror("mark6_sg_open");
		return -1;
	}

	/* Allocate read/write buffer */
	blocksize = mark6_sg_stripesize(fdi);
	if (blocksize < 1024)
	{
		fprintf(stderr, "Mark6 metadata has a strange stripe size of %zd. Using default.\n", blocksize);
		blocksize = 10*1024*1024;
	}
	if (posix_memalign((void**)&buf, 4096, blocksize) != 0)
	{
		perror("posix_memalign");
		mark6_sg_close(fdi);
		return -1;
	}
	Nreport = (off_t)(500*1024*1024/blocksize); // report every 500MB

	/* Perform the copying */
	mark6_sg_fstat(fdi, &st);
	ntotal  = st.st_size;
	nremain = st.st_size;
	if (do_progress) { fprintf(stderr, "Copying %.2f GB...\n", ntotal*1e-9); }
	while (nremain > 0)
	{
		size_t  rdsize;
		ssize_t nrd;
		ssize_t nwr;

		if (do_progress) { clock_gettime(CLOCK_REALTIME, &t0_rd); }
		rdsize = (nremain > blocksize) ? blocksize : nremain ;
		nrd = mark6_sg_pread(fdi, buf, rdsize, ncopied);
		if (do_progress) { clock_gettime(CLOCK_REALTIME, &t1_rd); }

		if (nrd <= 0)
		{
			fprintf(stderr, "Last read returned %zd. Done.\n", nrd);
			break;
		}

		if (do_progress) { clock_gettime(CLOCK_REALTIME, &t0_wr); }
		nwr = write(fdo, buf, nrd);
		if (nwr != nrd)
		{
			fprintf(stderr, "Write error (disk full?)\n");
			fprintf(stderr, "  %zd bytes written\n", ncopied + nwr);
			fprintf(stderr, "  %zd bytes remained to be written\n", nremain - nwr);
			break;
		}
		ncopied += nrd;
		nremain -= nrd;
		if (do_progress) { clock_gettime(CLOCK_REALTIME, &t1_wr); }

		if (do_progress)
		{
			T_mk6read += (t1_rd.tv_sec - t0_rd.tv_sec) + 1e-9*(t1_rd.tv_nsec - t0_rd.tv_nsec);
			T_write   += (t1_wr.tv_sec - t0_wr.tv_sec) + 1e-9*(t1_wr.tv_nsec - t0_wr.tv_nsec);
			if (((++Nops) % Nreport) == 0)
			{
				double R_read = 1e-6 * ((double)ncopied) / T_mk6read;
				double R_write = 1e-6 * ((double)ncopied) / T_write;
				fprintf(stderr,
					"Copied: %.2f/%.2f GB     "
					"Mark6 read: %.2f MB/s     "
					"File write: %.2f MB/s\n",
					ncopied*1e-9, ntotal*1e-9, R_read, R_write
				);
			}
		}
	}

	mark6_sg_close(fdi);
	close(fdo);

	return 0;
}

