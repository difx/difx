#ifdef __APPLE__

#define OSX

#define OPENREADOPTIONS O_RDONLY
#define OPENWRITEOPTIONS O_WRONLY|O_CREAT|O_TRUNC

#else

#define LINUX
#define _LARGEFILE_SOURCE 
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64

#define OPENREADOPTIONS O_RDONLY|O_LARGEFILE
#define OPENWRITEOPTIONS O_WRONLY|O_CREAT|O_TRUNC|O_LARGEFILE

#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>

#include "codifio.h"

#define BUFSIZE 10  // 10 MB

void rotate_band(unsigned char *buffer, ssize_t bytes_read, int bits, int dataSize, int nchan);

void handle_error(const char *msg) {
  perror(msg);
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {
  int nchan=0, bits=0, isComplex=0, dataSize=0, frameSize=0;
  char *output_dir = NULL, errormsg[1000];
  int opt;
  int i;

  struct option options[] = {
    {"outdir", 1, 0, 'd'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };
  
  // Parse command-line options
  while (1) {
    opt = getopt_long_only(argc, argv, "d:", options, NULL);
    if (opt==EOF) break;

    switch (opt) {

    case 'd':
      output_dir = optarg;
      break;

    case 'h':
      printf("Usage: %s [-d/-dir output_directory] file1 file2 ...\n", argv[0]);
      exit(EXIT_FAILURE);
    case '?':
      fprintf(stderr, "Usage: %s [-d output_directory] file1 file2 ...\n", argv[0]);
      exit(EXIT_FAILURE);
    
    default:
      ;
    }
  }
  
  if (optind >= argc) {
    fprintf(stderr, "Expected at least one file as argument\n");
    exit(EXIT_FAILURE);
  }

  // Process file in 10 MB chunks
  size_t alloc_bytes, bufsize;
  int nframe;
  alloc_bytes = BUFSIZE*1024*1024;
  unsigned char *buffer = malloc(alloc_bytes);
  if (!buffer) {
    handle_error("Memory allocation failed");
  }

  // Loop over each file provided in the arguments
  for (i = optind; i < argc; i++) {
    char *input_filename = argv[i];
    int fd = open(input_filename, O_RDWR);
    if (fd < 0) {
      sprintf(errormsg, "Error opening %s", input_filename);
      handle_error(errormsg);
    }
    
    // Read the codif header
    codif_header header_data, *header;
    header = &header_data; // Saves typing
    if (read(fd, header, CODIF_HEADER_BYTES) != CODIF_HEADER_BYTES) {
      handle_error("Error reading header");
    }

    nchan = getCODIFNumChannels(header);
    bits = getCODIFBitsPerSample(header);
    isComplex = getCODIFComplex(header);
    dataSize = getCODIFFrameBytes(header);
    frameSize = dataSize + CODIF_HEADER_BYTES;

    if (!isComplex) {
      fprintf(stderr, "Only works on complex data. Skipping %s\n", input_filename);
      close(fd);
      continue;
    }
    
    // Rewind file pointer to the beginning
    if (lseek(fd, 0, SEEK_SET) == (off_t)-1) {
      handle_error("Error seeking in file");
    }

    // Determine output file path
    char output_filename[1024];
    int out_fd;
    if (output_dir) {
      snprintf(output_filename, sizeof(output_filename), "%s/%s", output_dir, strrchr(input_filename, '/') ? strrchr(input_filename, '/') + 1 : input_filename);
      out_fd = open(output_filename, O_CREAT | O_TRUNC | O_WRONLY, 0644);
    } else {
      out_fd = fd; // In-place modification
    }

    if (out_fd < 0) {
      handle_error("Error opening output file");
    }

    // Ensure we are reading exact frames
    nframe = alloc_bytes / frameSize;
    bufsize = nframe * frameSize;
    
    ssize_t bytes_read;
    while ((bytes_read = read(fd, buffer, bufsize)) > 0) {
      if (bufsize % frameSize) { // Partial frame read
	fprintf(stderr, "Partial frameread - is the data corrupt?\n");
	close(fd);
	if (output_dir) close(out_fd);
	exit(1);
      }
      rotate_band(buffer, bytes_read, bits, dataSize, nchan); // Modify data in place

      if (!output_dir) {
	// Seek back to the correct position before writing
	if (lseek(fd, -bytes_read, SEEK_CUR) == (off_t)-1) {
	  handle_error("Error seeking back in file");
	}
      }

      if (write(out_fd, buffer, bytes_read) != bytes_read) {
	handle_error("Error writing output file");
      }
    }
    
    if (bytes_read < 0) {
      handle_error("Error reading file");
    }
    
    close(fd);
    if (output_dir) {
      close(out_fd);
    }
  }

  free(buffer);
  return 0;
}

void rotate_band(unsigned char *buffer, ssize_t bytes, int bits, int dataSize, int nchan) {
  int frameSize = dataSize + CODIF_HEADER_BYTES;
  int nframe = bytes / frameSize;

  int timeSamples = dataSize*8 / (bits*nchan);
  if (bits==8) {
    int i;
    int8_t *samples;
    if (nchan==1) {
      for (i=0; i<nframe; i++) {
        int j;
	samples = (int8_t*)&buffer[frameSize*i+CODIF_HEADER_BYTES];
	for (j=2; j<timeSamples; j+=4) {
	  samples[j] *= -1;
	  samples[j+1] *= -1;
	}
      }
    } else {
      fprintf(stderr, "Do not support 8 bit with %d channelsn", nchan);
      exit(1);
    }
  } else {
    fprintf(stderr, "Do not support %d bytes\n", bits);
    exit(1);
  }
}
