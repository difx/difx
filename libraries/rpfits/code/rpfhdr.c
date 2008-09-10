/*----------------------------------------------------------------------------
* List scan headers from an RPFITS file specified on the command line, or else
* on stdin, and print them as 80-character cards.
*
* This utility is independent of the RPFITS library and uses only the stdio
* C-library functions.
*
* Original: 2006/06/28 by Mark Calabretta, ATNF
* $Id: rpfhdr.c,v 1.3 2007/01/23 23:27:43 cal103 Exp $
*---------------------------------------------------------------------------*/

#define _LARGEFILE64_SOURCE

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef __APPLE__
#define freopen64 freopen
#define fseeko64 fseeko
#endif

int main(int argc, char **argv)

{
  char cbuff[2560], *cptr;
  char dashes[84] = "----------------------------------------"
                    "----------------------------------------";
  char equals[84] = "========================================"
                    "========================================";
  char spaces[84] = "                                        "
                    "                                        ";
  unsigned int iblock = 0, inhdr = 0, nblocks = 0, nhdr = 0, seekable = 1;
  unsigned long long int nbytes;

  if (argc > 1) {
    if (access(argv[1], R_OK) == -1) {
      perror(argv[1]);
      exit(1);
    }

    if (freopen64(argv[1], "r", stdin) == NULL) {
      perror(argv[1]);
      exit(1);
    }
  }

  while (1) {
    if (inhdr) {
      /* Get the next header block. */
      if (!fread(cbuff, (size_t)2560, (size_t)1, stdin)) break;
      iblock++;

    } else {
      /* Searching for a header. */
      if (!fread(cbuff, (size_t)10, (size_t)1, stdin)) break;

      if (strncmp(cbuff, "SIMPLE  = ", 10) == 0) {
        /* Found a header. */
        if (nblocks) {
          nbytes = 2560ll * nblocks;
          printf("Skipped %d blocks of data of size 2560 bytes (%lld "
            "bytes).   \n", nblocks, nbytes);
        }

        if (!fread(cbuff+10, (size_t)2550, (size_t)1, stdin)) break;

        printf("%s\nScan number %d at block number %d.\n%s\n",
          equals, ++nhdr, ++iblock, dashes);
        inhdr = 1;
        nblocks = 0;

      } else {
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
          if (!fread(cbuff+10, (size_t)2550, (size_t)1, stdin)) break;
        }

        iblock++;
      }
    }

    if (inhdr) {
      for (cptr = cbuff; cptr < cbuff + 2560; cptr += 80) {
        /* Write out a card. */
        printf("%.80s\n", cptr);

        /* Check for end-of-header. */
        if (strncmp(cptr, "END     ", 8) == 0) {
          inhdr = 0;
          printf("%s\n", dashes);
          fflush(stdout);
          break;
        }
      }

    } else {
      nblocks++;

      if (nblocks%1000 == 0) {
        /* Report progress on stderr in case it's saved to file. */
        nbytes = 2560ll * nblocks;
        fprintf(stderr, "Skipping %d blocks of data of size 2560 bytes "
          "(%lld bytes).   \r", nblocks, nbytes);
        fflush(stderr);
      }
    }
  }

  if (feof(stdin)) {
    nbytes = 2560ll * nblocks;
    printf("Skipped %d blocks of data of size 2560 bytes (%lld bytes).   \n",
      nblocks, nbytes);

    nbytes = 2560ll * iblock;
    printf("%s\nEnd-of-file after %d scan%s in %d x 2560-byte blocks "
      "(%lld bytes).\n%s\n", equals, nhdr, (nhdr > 1)?"s":"", iblock, nbytes,
      dashes);

    fprintf(stderr, "%s\r", spaces);

  } else {
    perror(argv[1]);
    exit(1);
  }

  exit(0);
}
