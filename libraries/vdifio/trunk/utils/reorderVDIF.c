#include <stdio.h>
#include <stdlib.h>
#include "vdifio.h"

const char program[] = "reorderVDIF";
const char author[]  = "Jan Wagner <jwagner@mpifr.de>";
const char version[] = "0.1";
const char verdate[] = "20190130";

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
  fprintf(stderr, "A program to read a VDIF file containing excess junk and write a cleaned up replacement.\n");
  fprintf(stderr, "\nUsage: %s <VDIF input file> <VDIF output file>\n", program);
  fprintf(stderr, "\n<VDIF input file> is the name of the VDIF file to read and clean\n");
  fprintf(stderr, "\n<VDIF output file> is the name of the VDIF file to write\n");
}

int main(int argc, char **argv)
{
  char buffer[128*1024 + 128];
  FILE * output;
  int n;

  struct vdif_file_summary info;
  struct vdif_file_reader reader;

  if(argc != 3)
  {
    usage();
    return EXIT_FAILURE;
  }

  n = summarizevdiffile(&info, argv[1], 0);
  if (n < 0)
  {
    fprintf(stderr, "Error: File %s VDIF summary failed with return value %d\n\n", argv[1], n);
    exit(EXIT_FAILURE);
  }
  fprintf(stderr, "The input VDIF seems to have %d threads. Processing...\n", info.nThread);

  output = fopen(argv[2], "w");
  if(output == NULL)
  {
    fprintf(stderr, "Error: Cannot open output file %s\n", argv[2]);
    exit(EXIT_FAILURE);
  }

  n = vdifreaderOpen(&info, &reader);
  if (n < 0)
  {
    fprintf(stderr, "Error: vdifreader_open() failed with return value %d\n\n", n);
    exit(EXIT_FAILURE);
  }

  while (1)
  {
    n = vdifreaderRead(&reader, buffer, sizeof(buffer));
    if (n == 0) break;
    fwrite(buffer, 1, sizeof(buffer), output);
  }

  vdifreaderClose(&reader);
  fclose(output);

  return EXIT_SUCCESS;
}

