#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <getopt.h>
#include <errno.h>
#include <inttypes.h>

#include <codifio.h>

int process_file(const char *filename, int threadID, int groupID) {
  int frameSize;
  off_t offset = 0;
  uint8_t buffer[CODIF_HEADER_BYTES];
  codif_header *header;

  header = (codif_header*)buffer;

  int fd = open(filename, O_RDWR);
  if (fd == -1) {
    perror("Error opening file");
    return -1;
  }

  while (1) {
    ssize_t bytes_read = pread(fd, header, CODIF_HEADER_BYTES, offset);
    if (bytes_read == -1) {
      perror("Error reading file");
      close(fd);
      return(-1);
    } else if (bytes_read == 0) {
      // End of file reached
      break;
    } else if (bytes_read != CODIF_HEADER_BYTES) {
      perror("Partial read of header\n");
      close(fd);
      return(-1);
    }

    if (offset==0) {
      frameSize = getCODIFFrameBytes(header);
    }
    
    if (threadID>0) {
      setCODIFThreadID(header, threadID);
    }
    if (groupID>0) {
      setCODIFGroupID(header, groupID);
    }
    
    // Write the modified data back to the file at the same position
    ssize_t bytes_written = pwrite(fd, buffer, CODIF_HEADER_BYTES, offset);
    if (bytes_written == -1) {
      perror("Error writing to file");
      close(fd);
      return -1;
    }
    
    // Move to the next block, skipping SKIP_SIZE bytes
    offset += CODIF_HEADER_BYTES+frameSize;
  }

  close(fd);
  return(0);
}

int main(int argc, char *argv[]) {
  int opt, status, tmp;
  int option_index = 0;

  int groupID = -1;
  int threadID = -1;

  int i;
    
  // Parse command line options with getopt
  static struct option options[] = {
    {"threadid", 1, 0, 't'},
    {"groupid", 1, 0, 'g'},
    {0, 0, 0, 0}
  };

  while ((opt = getopt_long_only(argc, argv, "t:g:", options, NULL)) != -1) {
    switch (opt) {

    case 't':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad threadid option %s\n", optarg);
      else 
	threadID = tmp;
     break;

    case 'g':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad groupid option %s\n", optarg);
      else 
	groupID = tmp;
     break;

    default:
      fprintf(stderr, "Fix codif headers\n");
      fprintf(stderr, "Usage: %s [-g GROUPID] [-t THREADID] <files>\n", argv[0]);
      exit(EXIT_FAILURE);
    }
  }

  for (i=optind; i<argc; i++) {
    if (i>optind) printf("\n");
    printf("%s:\n", argv[i]);
    
    process_file(argv[i], threadID, groupID);
  }
  
  return 0;
}
