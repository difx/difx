/*----------------------------------------------------------------------------
* rpfex [-o <outfile>] [-q] [-xI:J:K[,...]] [<infile>]
*-----------------------------------------------------------------------------
* Extract complete scans (header plus data) from an RPFITS file specified on
* the command line, or else on stdin.
 
* This utility is independent of the RPFITS library and uses only standard
* C-library functions.
*
* Original: 2006/07/10 by Mark Calabretta, ATNF
* $Id: rpfex.c,v 1.5 2007/01/23 23:27:43 cal103 Exp $
*---------------------------------------------------------------------------*/

char usage[] =
"Extract complete scans (header plus data) from an RPFITS file specified on\n"
"the command line, or else on stdin.\n"
"\n"
"Options:\n"
"  -o <file>    Write output to the specified file rather than stdout.\n"
"               The output file must differ from the input file!\n"
"\n"
"  -q           Run quietly, reporting only a summary of scans extracted,\n"
"               else progress messages are written to stderr.\n"
"\n"
"  -xI:J:K[,...]\n"
"               A colon-separated triple of integers that defines the start\n"
"               (I >= 1), end (J >= 0), and increment (K >= 1) for a\n"
"               sequence of scan numbers to be extracted.  If omitted, the\n"
"               start and increment both default to 1, and the end to 0\n"
"               (which means the last scan in the file).  Examples:\n"
"                 6           Scan 6\n"
"                 1:6         Scans 1,2,3,4,5,6\n"
"                 :6          Scans 1,2,3,4,5,6\n"
"                 7:          Scan 7 to end\n"
"                 1:6:2       Scans 1,3,5\n"
"                 ::2         All odd-numbered scans\n"
"                 :2,4:7,9:   All except scans 3 & 8\n"
"                 :           All scans (the default)\n"
"               As shown, any number of comma-separated triples may be\n"
"               specified.\n";

#define _LARGEFILE64_SOURCE

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifdef __APPLE__
#define freopen64 freopen
#define fseeko64 fseeko
#endif

int inrange(const char *range, const int num);
int checkrange(const char *range, int *last);
int gettriad(const char **rangep, int *start, int *end, int *incr);

int main(int argc, char **argv)

{
  char cbuff[2560], *outfile = 0, *range = ":";
  int  last;
  unsigned int i, iscan = 0, nblock = 0, nsel = 0, ntotal = 0, partial = 0,
       quiet = 0, seekable = 1, selected = 0;
  long long unsigned int nbytes;
  ino_t ino = 0;
  struct stat statst;

  /* Parse options. */
  for (i = 1; i < argc && argv[i][0] == '-'; i++) {
    switch (argv[i][1]) {
    case 'o':
      if (strlen(argv[i]) == 2) {
        outfile = &argv[++i][0];
      } else {
        outfile = &argv[i][2];
      }

      break;

    case 'q':
      quiet = 1;
      break;

    case 'x':
      if (strlen(argv[i]) == 2) {
        range = &argv[++i][0];
      } else {
        range = &argv[i][2];
      }

      if (checkrange(range, &last) == 0) {
        fprintf(stderr, "rpfex: Invalid range specification, \"%s\".\n",
          range);
        return 1;
      }
      break;

    default:
      fprintf(stderr, "\nUsage: rpfex [-o <outfile>] [-q] [-xI:J:K[,...]] "
        "[<infile>]\n\n%s\n", usage); 
      return 1;
    }
  }

  if (i < argc) {
    /* Check accessibility of the input file*/
    if (access(argv[i], R_OK) == -1) {
      perror(argv[i]);
      return 1;
    }

    /* Open the input file as stdin. */
    if (freopen64(argv[i], "r", stdin) == NULL) {
      perror(argv[i]);
      return 1;
    }

    if (outfile) {
      /* Get inode number of the input file. */
      if (stat(argv[i], &statst)) {
        perror(argv[i]);
        return 1;
      }

      ino = statst.st_ino;
    }
  }

  if (outfile) {
    /* Get inode number of the output file. */
    if (stat(outfile, &statst) == 0) {
      /* Output file already exists, check that it's not the input file. */
      if (ino == statst.st_ino) {
        fprintf(stderr, "%s: Output = input\n", outfile);
        return 1;
      }
    } else if (errno != ENOENT) {
      perror(outfile);
      return 1;
    }

    /* Open the output file as stdout. */
    if (freopen64(outfile, "w", stdout) == NULL) {
      perror(outfile);
      return 1;
    }
  }

  while (1) {
    if (selected || !seekable) {
      /* Get the next block. */
      if (!fread(cbuff, 2560, 1, stdin)) {
        if (!feof(stdin)) break;
      }
      partial = 0;

    } else {
      /* Searching for a header. */
      if (!fread(cbuff, (size_t)10, (size_t)1, stdin)) {
        if (!feof(stdin)) break;
      }
      partial = 1;
    }

    if (feof(stdin) || strncmp(cbuff, "SIMPLE  = ", 10) == 0) {
      /* Found a header or end-of-file. */
      nbytes = 2560llu * nblock;
      if (!quiet && iscan && nblock) fprintf(stderr, "Scan %2u,%6u x "
        "2560-byte blocks =%9llu bytes %s\n", iscan, nblock, nbytes,
        selected ? "extracted" : "skipped");

      if (feof(stdin)) break;

      iscan++;
      if (inrange(range, iscan)) {
        if (partial) {
          /* Read the rest of the block. */
          if (!fread(cbuff+10, (size_t)2550, (size_t)1, stdin)) break;
          partial = 0;
        }

        selected = 1;
        nsel++;

      } else {
        if (last && iscan > last) {
          if (!quiet) fprintf(stderr, "Scans remaining skipped\n");
          break;
        }

        selected = 0;
      }

      nblock = 0;
    }

    /* Consume the rest of the block if necessary. */
    if (partial) {
      /* Seek past it if possible. */
      if (seekable) {
        if (fseeko64(stdin, (off_t)2550, SEEK_CUR)) {
          if (errno == ESPIPE || errno == EBADF) {
            seekable = 0;
          } else {
            break;
          }
        }
      }

      if (!seekable) {
        /* Device is not seekable (pipe or tape), consume by reading. */
        if (!fread(cbuff+10, (size_t)2550, (size_t)1, stdin)) break;
      }
    }

    nblock++;
    if (selected) {
      if (fwrite(cbuff, 2560, 1, stdout) == 0) {
        fprintf(stderr, "rpfex: Write error.\n");
        return 1;
      }

      ntotal++;
    }

    if (!quiet && nblock%1000 == 0) {
      /* Report progress on stderr in case it's saved to file. */
      nbytes = 2560llu * nblock;
      fprintf(stderr, "Scan %2u,%6u x 2560-byte blocks =%9llu bytes %s\r",
        iscan, nblock, nbytes, selected ? "extracted" : "skipped");
      fflush(stderr);
    }
  }

  if (ferror(stdin)) {
    perror(outfile);
    exit(1);
  } else {
    nbytes = 2560llu * ntotal;
    fprintf(stderr, "Extracted %u scan%s in %u x 2560-byte blocks =%9llu "
      "bytes\n", nsel, (nsel==1) ? "" : "s", ntotal, nbytes);

  }

  return 0;
}

/*--------------------------------------------------------------------------*/

int inrange(char const *range, const int num)

{
  const char *cptr;
  int incr, end, start;

  cptr = range;
  while (gettriad(&cptr, &start, &end, &incr)) {
    if (num < start) {
      continue;
    }

    if (end && num > end) {
      continue;
    }

    if ((num - start) % incr) {
      continue;
    }

    return 1;
  }

  return 0;
}

/*--------------------------------------------------------------------------*/

int checkrange(const char *range, int *last)

{
  const char *cptr;
  int incr, end, start;

  cptr = range;
  if (*cptr == '\0') return 0;

  *last = -1;
  while (gettriad(&cptr, &start, &end, &incr)) {
    if (start < 1) return 0;
    if (incr  < 1) return 0;
    if (*last != 0 && *last < end) *last = end;
    if (*cptr == '\0') return 1;
  }

  return 0;
}

/*--------------------------------------------------------------------------*/

int gettriad(const char **rangep, int *start, int *end, int *incr)

{
  const char *cptr;

  *start = 1;
  *end   = 0;
  *incr  = 1;

  cptr = *rangep;

  /* Get start value. */
  if (*cptr != ':') {
    if (!isdigit(*cptr)) {
      return 0;
    }

    for (*start = 0; isdigit(*cptr); cptr++) {
      *start = (*start * 10) + (*cptr - '0');
    }

    if (*cptr == ',' || *cptr == '\0') {
      *end = *start;
      goto end;

    } else if (*cptr != ':') {
      return 0;
    }
  }


  /* Get end value. */
  cptr++;
  if (*cptr == ',' || *cptr == '\0') {
    goto end;
  } else if (*cptr != ':') {
    if (!isdigit(*cptr)) {
      return 0;
    }

    for (*end = 0; isdigit(*cptr); cptr++) {
      *end = (*end * 10) + (*cptr - '0');
    }

    if (*cptr == ',' || *cptr == '\0') {
      goto end;

    } else if (*cptr != ':') {
      return 0;
    }
  }


  /* Get increment. */
  cptr++;
  if (*cptr == ',' || *cptr == '\0') {
    goto end;
  } else {
    if (!isdigit(*cptr)) {
      return 0;
    }

    for (*incr = 0; isdigit(*cptr); cptr++) {
      *incr = (*incr * 10) + (*cptr - '0');
    }

    if (*cptr != ',' && *cptr != '\0') {
      return 0;
    }
  }

end:
  if (*cptr == ',') cptr++;
  *rangep = cptr;
  return 1;
}
