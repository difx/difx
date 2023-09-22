#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>

#define MAXCHAN 8
#define HEADERVERSION 1
#define HEADERSUBVERSION 2

#define HEADERSIZE 4096
#define TIMESTR 16
#define EXPERSTR 11
#define ANTENNASTR 21
#define ENUMSTR 10
#define VSTRSIZE 128

#define WHITESPACE " \t"
#define COMMENT_CHAR '#'

typedef enum {NONE=0, AT, VLBA} encodingtype;
typedef enum {NOPOL=0, L, R, X, Y} polarisationtype;
typedef enum {NOSIDE=0, USB, LSB} sidebandtype;

typedef struct vhead {
  char time[TIMESTR];
  char filetime[TIMESTR];
  char antennaid[2];
  char antennaname[ANTENNASTR];
  char experimentid[EXPERSTR];

  unsigned char headerversion;
  unsigned char headersubversion;
  unsigned char recorderversion;
  unsigned char recordersubversion;

  int headersize;
  int numbits;
  int nchan;                              // Number of recorded data channels
  int sequence;

  float bandwidth;                        // Bandwidth in MHz
  float frequency[MAXCHAN];               // Sky frequency in MHz

  encodingtype encoding;
  polarisationtype polarisation[MAXCHAN]; // Channel polarisation
  sidebandtype sideband[MAXCHAN];         // Channel sideband

} vhead;

/* Error codes */

#define NOERROR   0  // Everything OK
#define HEADSIZE  1  // Header line expands to be too long. Maybe increase STRSIZE
#define STRLENERR 2   // Passed too long string
#define HEADEOF   3   // EOF while reading headers
#define BADVALUE  4   // Bad value while reading header
#define NOVALUE   5   // No value associated with keyword
#define LONGSTR   6   // Passed string too long
#define NCHAN     7   // Nchan greater than MAXCHAN
#define NOCHAN    8   // Nchan not set
#define BADTIME   9   // Badly formated time cod
#define FILEOPENERROR   10   // Error opening file
#define FILEWRITEERROR  11   // Error writing file 
#define FILEWRITEERROR1 12   // Error writing file (too few bytes)
#define FILEWRITEERROR2 13   // Error writing file (Nothing written)
#define HEADERSEEKERROR 14   // Failed to seek to end of header

typedef struct enumstr {
  int val;
  char str[ENUMSTR];
} enumstr;

#define ENDENUM -1

extern enumstr encodestrs[];

int resetheader(vhead *header);
vhead *newheader ();
void destroyheader(vhead *header);
int settime(vhead *header, struct tm *date);
int settimestr(vhead *header, char *date);
int setfiletime(vhead *header, struct tm *date);
int setfiletimestr(vhead *header, char *date);
int setantennaid(vhead *header, const char* antennaid);
int setantennaname(vhead *header, const char* name);
int setexperimentid(vhead *header, const char* exper);
int setheaderversion(vhead *header, int major, int minor);
int setrecorderversion(vhead *header, int major, int minor);
int setheadersize(vhead *header, int size);
int setnumbits(vhead *header, int nbits);
int setnchan(vhead *header, int nchan);
int setsequence(vhead *header, int sequence);
int setbandwidth(vhead *header, float bandwidth);
int setencoding(vhead *header, encodingtype encoding);
int setfrequency(vhead *header, float freq[]);
int setpolarisation(vhead *header, polarisationtype pol[]);
int setsideband(vhead *header, sidebandtype side[]);
int writeheader(vhead *header, int file, char **buf);
int readheader(vhead *header, int file, char *buf);
int readprofile(vhead *header, const char *profilename);

int gettime(vhead *header, struct tm *date);
int gettimestr(vhead *header, char *date);
int getfiletime(vhead *header, struct tm *date);
int getfiletimestr(vhead *header, char *date);

char* enum2str(int val, enumstr *enumarray);
double tm2mjd(struct tm date);
double cal2mjd(int day, int month, int year);
