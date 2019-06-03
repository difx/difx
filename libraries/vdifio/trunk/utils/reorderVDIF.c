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
  const size_t buffer_size = 8224*16*1024;
  char *buffer;
  FILE * output;
  int n;

  struct vdif_file_summary info;
  struct vdif_file_reader reader;
  struct vdif_file_reader_stats st;

  buffer = (char*)malloc(buffer_size);

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
    size_t nrd;
    nrd = vdifreaderRead(&reader, buffer, buffer_size);
    if (nrd == 0) break;
    fwrite(buffer, 1, nrd, output);
    vdifreaderStats(&reader, &st);
    printf("VDIF reader statistics: read %zu of %zu\n", nrd, buffer_size);
    for(n = 0; n < st.nThread; n++)
    {
      printf("  Thread %2d relative offset = %5d frames, sec=%d, eof=%d\n", info.threadIds[n], st.threadOffsets[n], reader.sec[n], reader.feof[n]);
    }
    printf("  Virtual offset = %zd\n", reader.virtualoffset);
    printf("  Frame size     = %d\n", reader.details.frameSize);
    printf("  Frame rate     = %d fps/thread\n", reader.fps);
    printf("  Largest offset = %d frames\n", st.maxOffset);
  }

  vdifreaderClose(&reader);
  fclose(output);
  free(buffer);

  return EXIT_SUCCESS;
}

