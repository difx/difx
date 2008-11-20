/*----------------------------------------------------------------------------
* Usage: fitshdr [infile]
*-----------------------------------------------------------------------------
* List headers from a FITS file specified on the command line, or else on
* stdin, printing them as 80-character cards without trailing blanks.
*
* If invoked as 'rpfhdr' rather than 'fitshdr' it also handles RPFITS format
* which has a block size of 2560 (rather than 2880) but otherwise looks like
* FITS.
*
* Handles large files (>2GiB) using explicit LFS function calls, i.e. with
* _LARGEFILE64_SOURCE defined, but apparently Macs support large files
* natively, c.f. http://developer.apple.com/documentation/Darwin/Reference/-
* Manpages/man3/freopen.3.html in the "Compatibility" section, in which case
* these are defined back to vanilla stdio calls.
*
* Original: 2006/06/28 by Mark Calabretta, ATNF
* $Id: rpfhdr.c,v 1.6 2007/06/27 05:24:09 cal103 Exp $
*---------------------------------------------------------------------------*/

#define _LARGEFILE64_SOURCE

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef __APPLE__
/* Macs handle large files natively. */
#define freopen64 freopen
#define fseeko64  fseeko
#define off64_t   off_t
#endif

int main(int argc, char **argv)

{
  char cbuff[2880], *cptr;
  char dashes[84] = "----------------------------------------"
                    "----------------------------------------";
  char equals[84] = "========================================"
                    "========================================";
  char spaces[84] = "                                        "
                    "                                        ";
  char *format, rpfits[8] = "RPFITS";
  int  len;
  unsigned int blksiz = 2880, inhdr = 0, nhdr = 0, seekable = 1;
  unsigned long long int iblock = 0, nblock = 0, nbyte;

  /* If an input file name was specified then reopen it as stdin */
  /* (doesn't affect seekability).                               */
  if (argc > 1) {
    if (access(argv[1], R_OK) == -1) {
      perror(argv[1]);
      return 2;
    }

    if (freopen64(argv[1], "r", stdin) == NULL) {
      perror(argv[1]);
      return 2;
    }
  }

  /* Check for standard FITS or RPFITS. */
  if (!fread(cbuff, (size_t)80, (size_t)1, stdin)) {
    perror(argv[1]);
    return 2;
  }

  if (strncmp(cbuff, "SIMPLE  = ", 10) == 0) {
    if (!fread(cbuff+80, (size_t)80, (size_t)1, stdin)) {
      perror(argv[1]);
      return 2;
    }

    /* Assume FITS by default. */
    format = rpfits + 2;
    blksiz = 2880;

    /* Check for RPFITS. */
    if (strncmp(cbuff+80, "FORMAT  =               RPFITS", 30) == 0 ||
        strncmp(cbuff+80, "FORMAT  =             'RPFITS'", 30) == 0) {
      if (strcmp(*argv, "rpfhdr") == 0) {
        /* If invoked as 'rpfhdr' then allow RPFITS. */
        format = rpfits;
        blksiz = 2560;
      } else {
        /* Otherwise disallow RPFITS but issue a warning. */
        printf("WARNING: Input appears to be RPFITS, continuing anyway using "
          "2880-byte blocks.\n" );
      }
    }

    /* Read the rest of the first block. */
    if (!fread(cbuff+160, (size_t)(blksiz-160), (size_t)1, stdin)) {
      perror(argv[1]);
      return 2;
    }

    inhdr = 1;

  } else {
    /* If we have not been invoked as 'rpfhdr' then bail out now. */
    if (strcmp(*argv, "rpfhdr") != 0) {
      fprintf(stderr, "Input file does not appear to be standard FITS.\n" );
      return 1;
    }

    /* RPFITS may have a block or two of rubbish before the first header. */
    format = rpfits;
    blksiz = 2560;

    if (!fread(cbuff+80, (size_t)(blksiz-80), (size_t)1, stdin)) {
      perror(argv[1]);
      return 2;
    }

    while (iblock < 4) {
      if (!fread(cbuff, (size_t)blksiz, (size_t)1, stdin)) {
        perror(argv[1]);
        return 2;
      }

      iblock++;
      if (strncmp(cbuff, "SIMPLE  = ", 10) == 0) {
        inhdr = 1;
        break;
      }
    }

    if (!inhdr) {
      fprintf(stderr, "Input does not appear to be FITS or RPFITS.\n" );
      return 1;
    }

    if (iblock) {
      nbyte = blksiz * iblock;
      printf("Skipped %lld block%s of rubbish of size %d bytes (%lld "
        "bytes).\n", iblock, (iblock > 1)?"s":"", blksiz, nbyte);
    }
  }

  printf("%s\n%s header number %d at block number %lld.\n%s\n", equals,
    format, ++nhdr, ++iblock, dashes);


  /* Scan through the file. */
  while (1) {
    if (!inhdr) {
      /* Searching for a header. */
      if (!fread(cbuff, (size_t)10, (size_t)1, stdin)) break;

      if (strncmp(cbuff, "SIMPLE  = ", 10) == 0 ||
          strncmp(cbuff, "XTENSION= ", 10) == 0) {
        /* Found a header. */
        if (iblock) {
          nbyte = blksiz * nblock;
          printf("Skipped %lld block%s of data of size %d bytes (%lld "
            "bytes).\n", nblock, (nblock == 1)?"":"s", blksiz, nbyte);
        }

        if (!fread(cbuff+10, (size_t)(blksiz-10), (size_t)1, stdin)) break;

        printf("%s\n%s header number %d at block number %lld.\n%s\n",
          equals, format, ++nhdr, ++iblock, dashes);
        inhdr = 1;
        nblock = 0;

      } else {
        /* Seek past it if possible. */
        if (seekable) {
          if (fseeko64(stdin, (off64_t)(blksiz-10), SEEK_CUR)) {
            if (errno == ESPIPE || errno == EBADF) {
              seekable = 0;
            } else {
              break;
            }
          }
        }

        if (!seekable) {
          if (!fread(cbuff+10, (size_t)(blksiz-10), (size_t)1, stdin)) break;
        }

        iblock++;
      }
    }

    if (inhdr) {
      for (cptr = cbuff; cptr < cbuff + blksiz; cptr += 80) {
        /* Write out a card without trailing blanks. */
        for (len = 80; len > 0; len--) {
          if (cptr[len-1] != ' ') break;
        }
        printf("%.*s\n", len, cptr);

        /* Check for end-of-header. */
        if (strncmp(cptr, "END     ", 8) == 0) {
          inhdr = 0;
          printf("%s\n", dashes);
          fflush(stdout);
          break;
        }
      }

      /* Get the next header block. */
      if (inhdr) {
        if (!fread(cbuff, (size_t)blksiz, (size_t)1, stdin)) break;
        iblock++;
      }

    } else {
      nblock++;

      if (nblock%1000 == 0) {
        /* Report progress on stderr in case it's saved to file. */
        nbyte = blksiz * nblock;
        fprintf(stderr, "Skipping %lld blocks of data of size %d bytes "
          "(%lld bytes).   \r", nblock, blksiz, nbyte);
        fflush(stderr);
      }
    }
  }

  if (feof(stdin)) {
    nbyte = blksiz * nblock;
    printf("Skipped %lld block%s of data of size %d bytes (%lld bytes). \n",
      nblock, (nblock == 1)?"":"s", blksiz, nbyte);

    nbyte = blksiz * iblock;
    printf("%s\nEnd-of-file after %d HDU%s in %lld x %d-byte blocks (%lld "
      "bytes).\n%s\n", equals, nhdr, (nhdr == 1)?"":"s", iblock, blksiz,
      nbyte, dashes);

    fprintf(stderr, "%s\r", spaces);

  } else {
    perror(argv[1]);
    return 2;
  }

  return 0;
}
