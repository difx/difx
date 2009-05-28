#define SENDOPTIONS  0

#ifdef __APPLE__
#define OSX
#elif defined(__FreeBSD__)
#define FREEBSD
#elif defined(__linux__)
#define LINUX
#undef SENDOPTIONS
#define SENDOPTIONS MSG_NOSIGNAL
#else
#define GENERIC
#endif

#include <sys/types.h>
#include <time.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>     
#include <sys/timeb.h>
#include <sys/time.h>       
#include <sys/socket.h>  
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <getopt.h>

#include "vheader.h"
#include "mk5blib.h"

#define DEFAULT_BUFSIZE    1000  //  KB
#define MAXSTR              200 

#define DEBUG(x) 

typedef struct udp {
  int enabled;
  int size;
  int extra;
  unsigned long long sequence;
  u_int64_t usleep;
} udp;

double tim(void);
void kill_signal (int);
int setup_net(char *hostname, int port, int window_size, udp *udp, int *sock);
void throttle_rate (double firsttime, float datarate, 
		    unsigned long long totalsize, double *tbehind);
double currentmjd ();
void mjd2cal(double mjd, int *day, int *month, int *year, double *ut);
int cal2dayno(int day, int month, int year);
void dayno2cal(int dayno, int year, int *day, int *month);
double cal2mjd(int day, int month, int year);
int netsend(int sock, char *buf, size_t len, udp *udp);
void my_usleep(u_int64_t usec);

int main (int argc, char * const argv[]) {
  unsigned short fnamesize;
  ssize_t ntowrite;
  int i, status, ofile, opt, tmp, sock, thisday, thismonth, thisyear, bwrote;
  int seconds, hour, min, sec, bufsize, datarate;
  char msg[MAXSTR+50], filetimestr[MAXSTR];
  char *buf, *headbuf, *ptr;
  double mjd, finishmjd, ut, tbehind, t0, t1, t2;
  float ftmp, speed;
  unsigned long long filesize, networksize, nwritten, totalsize;
  vhead *header;    // File header object
  mk5bheader mk5_header;

  //int monitor = 0;  
  int port = 52100;    /* Write nfiles files of size total size */
  int window_size = -1;	/* kilobytes */
  //int datarate = 512; /* Mbps */
  int year = -1;
  int month = -1;
  int day = -1;
  int dayno = -1;
  int numchan = 4;
  int mark5b = 0;
  int bits=2;
  int bandwidth=16;
  //float rate = 0; /* Limit read/write to this data rate */
  float updatetime = 1; /* 1 second update by default */
  float filetime = 1; /* 1 second "files" */
  double duration = 60; /* 10min by default */
  char filebase[MAXSTR+1]; /* Base name for output file */
  char hostname[MAXSTR+1] = ""; /* Host name to send data to */
  char timestr[MAXSTR] = "";

  udp udp;
  udp.size = 0;
  udp.enabled = 0;  
  udp.usleep = 0;  

  struct option options[] = {
    {"duration", 1, 0, 'd'},
    {"bandwidth", 1, 0, 'W'},
    {"day", 1, 0, 'D'},
    {"dayno", 1, 0, 'Y'},
    {"month", 1, 0, 'm'},
    {"year", 1, 0, 'y'},
    {"time", 1, 0, 't'},
    {"port", 1, 0, 'p'},
    {"host", 1, 0, 'H'},
    {"update", 1, 0, 'u'},
    {"window", 1, 0, 'w'},
    {"blocksize", 1, 0, 'b'},
    {"filetime", 1, 0, 'f'},
    {"udp", 1, 0, 'U'},
    {"sleep", 1, 0, 's'},
    {"usleep", 1, 0, 's'},
    {"nchan", 1, 0, 'n'},
    {"mark5b", 0, 0, 'B'},
    {"mk5b", 0, 0, 'B'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };

  bufsize  = DEFAULT_BUFSIZE * 1024;
  ofile = -1;
  i = -1;

  setenv("TZ", "", 1); 
  tzset();

  setlinebuf(stdout);
  
  /* Read command line options */
  while (1) {
    opt = getopt_long_only(argc, argv, "r:n:DVvdmhH:", 
			   options, NULL);
    if (opt==EOF) break;

    switch (opt) {
      
    case 'b':
      status = sscanf(optarg, "%f", &ftmp);
      if (status!=1)
	fprintf(stderr, "Bad blocksize option %s\n", optarg);
      else 
	bufsize = ftmp * 1024;
     break;

    case 'f':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1 || tmp==0)
	fprintf(stderr, "Bad filesize option %s\n", optarg);
      else 
	filetime = tmp;
     break;

    case 'w':
      status = sscanf(optarg, "%f", &ftmp);
      if (status!=1)
	fprintf(stderr, "Bad window option %s\n", optarg);
      else 
	window_size = ftmp * 1024;
     break;
     
    case 'r':
      status = sscanf(optarg, "%f", &ftmp);
      if (status!=1)
     	fprintf(stderr, "Bad bandwidth option %s\n", optarg);
      else 
     	bandwidth = ftmp;
      break;

    case 'p':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad port option %s\n", optarg);
      else {
	port = tmp;
      }
      break;
      
    case 'd':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad duration option %s\n", optarg);
      else {
	duration = tmp;
      }
      break;

    case 'D':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad day option %s\n", optarg);
      else {
	day = tmp;
      }
      break;

    case 'Y':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad dayno option %s\n", optarg);
      else {
	dayno = tmp;
      }
      break;

    case 'n':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad nchan option %s\n", optarg);
      else {
	numchan = tmp;
      }
      break;
      
    case 'm':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad month option %s\n", optarg);
      else {
	month = tmp;
      }
      break;
      
    case 'y':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad year option %s\n", optarg);
      else {
	year = tmp;
      }
      break;
      
    case 't':
      strcpy(timestr, optarg);
      break;
      
    case 'u':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad time update option %s\n", optarg);
      else 
	updatetime = tmp;
      break;
      
    case 's':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad usleep option %s\n", optarg);
      else 
	udp.usleep = tmp;
      break;
      
    case 'H':
      if (strlen(optarg)>MAXSTR) {
	fprintf(stderr, "Hostname too long\n");
	return(1);
      }
      strcpy(hostname, optarg);
      break;

    case 'U':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad udp option %s\n", optarg);
      else {
	udp.enabled = 1;
	udp.size = tmp;
      }
      break;
      
    case 'B':
      mark5b = 1;
      break;

    case 'h':
      printf("Usage: vlbi_fake [options]\n");
      printf("  -H/host <HOSTNAME>    Remote host to connect to\n");
      printf("  -p/-port <PORT>       Port number for transfer\n");
      printf("  -d/duration <DUR>     Time in (transferred) seconds to run (60)\n");
      printf("  -bandwidth <BANWIDTH> Channel bandwidth in MHz (16)\n");
      printf("  -n/-nchan <N>         Number of 2 bit channels to assume in stream\n");
      printf("  -day <DAY>            Day of month of start time (now)\n");
      printf("  -month <MONTH>        Month of start time (now)\n");
      printf("  -dayno <DAYNO>        Day of year of start time (now)\n");
      printf("  -year <YEAR>          Year of start time (now)\n");
      printf("  -time <HH:MM:SS>      Year of start time (now)\n");
      printf("  -mark5b/mk5b          Send Mark5b format data\n");
      printf("  -udp <MTU>            Use UDP with given datagram size (Mark5b only)\n");
      printf("  -sleep <USEC>         Sleep (usec) between udp packets\n");
      printf("  -u/-update <SEC>      Number of seconds to average timing statistics\n");
      printf("  -w/-window <SIZE>     TCP window size (kB)\n");
      printf("  -b/blocksize <SIZE>   Blocksize to write, kB (1 MB default)\n");
      printf("  -f/filesize <SIZE>    Size in sec for files (1)\n");
      printf("  -h/-help              This list\n");
      return(1);
    break;
    
    case '?':
    default:
      break;
    }
  }

  if (mark5b) {
    bufsize=10000+MK5BHEADSIZE*4;
    udp.extra = 0;
  }

  if (udp.enabled) {
    if (!mark5b) {
      fprintf(stderr, "Can only run UDP in mark5b mode\n");
      exit(1);
    }
    udp.size -= 20; // IP header
    udp.size -= 4*2; // UDP header
    udp.size -= sizeof(long long); // Sequence number
    udp.size &= ~0x7;  //Truncate to smallest multiple of 8
      
    if (udp.size<=0) {
      printf("Error: Specified UDP MTU size (%d) too small\n", 
	     udp.size+20+4*2+(int)sizeof(long long));
      exit(1);
    }
    bufsize+=udp.size;
    udp.sequence = 0;
  }

  strcpy(filebase, "TESTFILE");

  if (strlen(hostname) == 0) {
    strcpy(hostname, "localhost");
  }

  if (updatetime<0.25) updatetime = 0.25;

  duration /= 60*60*24;


  // Initialise buffers and syncronisation variables
  buf = malloc(bufsize);
  if (buf==NULL) {
    sprintf(msg, "Trying to allocate %d KB", bufsize/1024);
    perror(msg);
    return(1);
  }

  totalsize=0;

  if (!mark5b) {
    header = newheader();

    setantennaid(header, "Tt");
    setantennaname(header, "Test Antenna");
    setexperimentid(header, "VTXXXX");
    setrecorderversion(header, 0, 0);
    setbandwidth(header, bandwidth);
    setnchan(header, numchan);
    setnumbits(header, bits);
    setencoding(header, AT);
    setsequence(header, 0);
    strcpy(buf, "Chris was here");

  } else {
    header = NULL; 
    strcpy(buf+MK5BHEADSIZE+4, "Chris was here");
  }
  tbehind = 0;

  mjd = currentmjd();
  mjd2cal(mjd, &thisday, &thismonth, &thisyear, &ut);

  if (year==-1) year = thisyear;
  if (day==-1) day = thisday;
  if (month==-1) month = thismonth;
  if (dayno!=-1) dayno2cal(dayno, year, &day, &month);

  if (strlen(timestr)>0) {
    status = sscanf(timestr, "%d:%d:%d", &hour, &min, &sec);
    if (status!=3) {
      fprintf(stderr, "Could not parse time %s\n", timestr);
      exit(1);
    }
    ut = (hour+(min+sec/60.0)/60.0)/24.0;
  }

  mjd = cal2mjd(day, month, year)+ut;

  finishmjd = mjd+duration;

  datarate = numchan*bits*bandwidth*2;

  if (mark5b) {
    init_bitreversal();
  
    initialise_mark5bheader(&mk5_header, datarate, mjd);

    filesize = MK5BFRAMESIZE+MK5BHEADSIZE*4;
  } else {
    filesize = filetime*datarate*1e6/8;  // Seconds and Mbps => bytes
    filetime = filesize/(datarate*1e6)*8;  // Rounding
    filetime /= 60*60*24;
  }

  status = setup_net(hostname, port, window_size, &udp, &sock);
  if (status) return(1);

  t0 = tim();
  t1 = t0;

  bwrote = 0;
  while (mjd+0.001/60/60/24<finishmjd) {
    
    if (mark5b) {
      next_mark5bheader(&mk5_header);

      ptr = buf+udp.extra;
      memcpy(ptr, mk5_header.header, MK5BHEADSIZE*4);

    } else { // vsi 
      // Create vsi header
      mjd2cal(mjd, &day, &month, &year, &ut);
      dayno = cal2dayno(day, month, year);

      seconds = (int)(ut*60*60*24+0.5);
      hour = seconds/3600;
      sec = seconds %60;
      min = (seconds-hour*3600)/60;
      sprintf(timestr, "%04d%02d%02d:%02d%02d%02d", 
	      year, month, day, hour, min, sec);
      status = settimestr(header, timestr);
      if (status!=NOERROR) {
	fprintf(stderr, "Error: Writing time header failed\n");
	exit(1);
      }

      writeheader(header, 0, &headbuf);
      header->sequence++;

      //sprintf(filename, "%s%d", filebase, ifile);
      sprintf(filetimestr, "%s-%03d_%02d%02d%02d.lba", filebase, dayno,
	      hour, min, sec);
      printf("Sending %s\n", filetimestr);
      fnamesize = strlen(filetimestr)+1;
    
      // Send Network Header
      ptr = buf;
      networksize = filesize+header->headersize;
      memcpy(ptr, &networksize, sizeof(long long));
      ptr += sizeof(long long);
      memcpy(ptr, &fnamesize, sizeof(short));
      ptr += sizeof(short);
      memcpy(ptr, filetimestr, fnamesize);

      ntowrite = sizeof(long long)+sizeof(short)+fnamesize;

      status = netsend(sock, buf, ntowrite, &udp);
      if (status) exit(1);
  
      // Send VSI header
      status = netsend(sock, headbuf, header->headersize, &udp);
      if (status) exit(1);

      free(headbuf);
      strcpy(buf, "Chris was here                                       ");
    }

    // Send data buffer
    nwritten = 0;
    while (nwritten<filesize) {
      ntowrite = filesize-nwritten;
      if (ntowrite>bufsize) ntowrite = bufsize;

      status = netsend(sock, buf, ntowrite, &udp);
      if (status) exit(1);

      nwritten += ntowrite;
      bwrote += ntowrite;

    }
    t2 = tim();

    if (t2-t1>updatetime) {

      speed = bwrote/(t2-t1)/1e6*8;
      printf("%6.1f Mbps/s %.1f sec  %5.1f MB\n", speed, t2-t1, bwrote/1e6);

      t1 = t2;
      totalsize += bwrote;
      bwrote = 0;
    }
    
    if (mark5b) {
      mjd = mark5b_mjd(&mk5_header);
    } else {
      mjd += filetime;
    }
  }

  totalsize += bwrote;

  t2 = tim();

  status = close(sock);
  if (status!=0) {
    perror("Error closing socket");
  }
    
  speed = totalsize/(t2-t0)/1e6*8;
  printf("\n  Rate = %.2f Mbps/s (%.1f sec)\n\n", speed, t2-t0);

  return(0);
}
  
double tim(void) {
  struct timeval tv;
  double t;

  gettimeofday(&tv, NULL);
  t = (double)tv.tv_sec + (double)tv.tv_usec/1000000.0;

  return t;
}

int setup_net(char *hostname, int port, int window_size, udp *udp, int *sock) {
  int status;
  unsigned long ip_addr;
  socklen_t winlen;
  struct hostent     *hostptr;
  struct sockaddr_in server;    /* Socket address */

  hostptr = gethostbyname(hostname);
  if (hostptr==NULL) {
    fprintf(stderr,"Failed to look up hostname %s\n", hostname);
    return(1);
  }
  
  memcpy(&ip_addr, (char *)hostptr->h_addr, sizeof(ip_addr));
  memset((char *) &server, 0, sizeof(server));
  server.sin_family = AF_INET;
  server.sin_port = htons((unsigned short)port); 
  server.sin_addr.s_addr = ip_addr;
  
  printf("Connecting to %s\n",inet_ntoa(server.sin_addr));
    

  if (udp->enabled) {
    *sock = socket(AF_INET,SOCK_DGRAM, IPPROTO_UDP); 
    if (*sock==-1) {
      perror("Failed to allocate UDP socket");
      return(1);
    }
  } else {
    *sock = socket(AF_INET, SOCK_STREAM, 0);
    if (*sock==-1) {
      perror("Failed to allocate socket");
      return(1);
    }

    if (window_size>0) {
      status = setsockopt(*sock, SOL_SOCKET, SO_SNDBUF,
			  (char *) &window_size, sizeof(window_size));
      if (status!=0) {
	close(*sock);
	perror("Setting socket options");
	return(1);
      }
      status = setsockopt(*sock, SOL_SOCKET, SO_RCVBUF,
			  (char *) &window_size, sizeof(window_size));
      if (status!=0) {
	close(*sock);
	perror("Setting socket options");
	return(1);
      }

      /* Check what the window size actually was set to */
      winlen = sizeof(window_size);
      status = getsockopt(*sock, SOL_SOCKET, SO_SNDBUF,
			  (char *) &window_size, &winlen);
      if (status!=0) {
	close(*sock);
	perror("Getting socket options");
	return(1);
      }
      printf("Sending TCP buffersize set to %d Kbytes\n", window_size/1024);
    }
  }

  status = connect(*sock, (struct sockaddr *) &server, sizeof(server));
  if (status!=0) {
    perror("Failed to connect to server");
    return(1);
  }
  
  return(0);
}

int netsend(int sock, char *buf, size_t len, udp *udp) {
  if (udp->enabled) {
    char *ptr, str[256];
    struct msghdr msg;
    struct iovec  iovect[2];
    ssize_t nsent, ntosend;
  
    msg.msg_name       = 0;
    msg.msg_namelen    = 0;
    msg.msg_iov        = &iovect[0];
    msg.msg_iovlen     = 2;
    msg.msg_control    = 0;
    msg.msg_controllen = 0;
    msg.msg_flags      = 0;
  
    ntosend = sizeof(long long) + udp->size;

    iovect[0].iov_base = &udp->sequence;
    iovect[0].iov_len  = sizeof(long long);
    iovect[1].iov_len  = udp->size;
  
    ptr = buf;
    while (ptr+udp->size<buf+len) {
      iovect[1].iov_base = ptr;

      nsent = sendmsg(sock, &msg, MSG_EOR);
      if (nsent==-1) {
	sprintf(str, "Sending %d byte UDP packet: ", (int)ntosend);
	perror(str);
	return(1);
      } else if (nsent!=ntosend) {
	printf("Only sent %d of %d bytes for UDP packet\n", (int)nsent, (int)ntosend);
	return(1);
      }
      udp->sequence++;
      ptr+=udp->size;

      if (udp->usleep>0) {
	my_usleep(udp->usleep);
      }
    }

    udp->extra= buf+len-ptr;
    memcpy(buf, ptr, udp->extra);
    
  } else {
    char *ptr;
    int ntowrite, nwrote;
  
    ptr = buf;
    ntowrite = len;

    while (ntowrite>0) {
      nwrote = send(sock, ptr, ntowrite, SENDOPTIONS);
      if (nwrote==-1) {
	if (errno == EINTR) continue;
	perror("Error writing to network");
	return(1);
      } else if (nwrote==0) {
	printf("Warning: Did not write any bytes!\n");
	return(1);
      } else {
	ntowrite -= nwrote;
	ptr += nwrote;
      }
    }
  }
  return(0);
}

int udpsend(int sock, int datagramsize, char *buf, int *bufsize, 
	    unsigned long long *sequence) {
  char *ptr, str[256];

  struct msghdr msg;
  struct iovec  iovect[2];
  ssize_t nsent, ntosend;
  
  msg.msg_name       = 0;
  msg.msg_namelen    = 0;
  msg.msg_iov        = &iovect[0];
  msg.msg_iovlen     = 2;
  msg.msg_control    = 0;
  msg.msg_controllen = 0;
  msg.msg_flags      = 0;
  
  ntosend = sizeof(long long) + datagramsize;

  iovect[0].iov_base = sequence;
  iovect[0].iov_len  = sizeof(long long);
  iovect[1].iov_len  = datagramsize;
  
  ptr = buf;
  while (ptr+datagramsize<buf+*bufsize) {
    iovect[1].iov_base = ptr;

    nsent = sendmsg(sock, &msg, 0);
    if (nsent==-1) {
      sprintf(str, "Sending %d byte UDP packet: ", (int)ntosend);
      perror(str);
      return(1);
    } else if (nsent!=ntosend) {
      printf("Only sent %d of %d bytes for UDP packet\n", (int)nsent, (int)ntosend);
      return(1);
    }
    *sequence +=1;
    ptr+=datagramsize;
  }

  *bufsize = buf+*bufsize-ptr;
  memcpy(buf, ptr, *bufsize);

  return(0);
}

void throttle_rate (double firsttime, float rate, 
		    unsigned long long totalsize, double *tbehind) {
  /* Sleep until time catches up with us, assuming we are writing faster than
     requested */
  int status;
  double t2, dt, twait, expected_time;

  t2 = tim();
  dt = t2-firsttime;

  expected_time = totalsize/(float)rate;
  twait = (expected_time-dt);

  if (twait>0) {
    *tbehind = 0;
    status = usleep(twait*1e6);
    if ((status!=0) & (status!= EINTR)) {
      perror("Calling usleep\n");
      exit(1);
    }
  } else {
    twait *= -1;
    //if ((-twait>1) & (abs(twait-*tbehind)>0.1)) { 
    if ((abs(twait-*tbehind)>1)) { 
      /* More than a second difference */ 
      *tbehind = twait;
      printf(" Dropping behind %.1f seconds\n", twait);
    }
  }
  return;
}
  
double cal2mjd(int day, int month, int year) {
  int m, y, c, x1, x2, x3;

  if (month <= 2) {
    m = month+9;
    y = year-1;
  } else {
    m = month-3;
    y = year;
  }

  c = y/100;
  y = y-c*100;

  x1 = 146097*c/4;
  x2 = 1461*y/4;
  x3 = (153*m+2)/5;

  return(x1+x2+x3+day-678882);
}

void mjd2cal(double mjd, int *day, int *month, int *year, double *ut) {
  int jd, temp1, temp2;

  *ut = fmod(mjd,1.0);

  if (*ut<0.0) {
    *ut += 1.0;
    mjd -= 1;
  }

  jd = (int)floor(mjd + 2400001);
  //printf("mjd=%.1f\n",mjd);
  //printf("jd=%d\n",jd);

  // Do some rather cryptic calculations
  
  temp1 = 4*(jd+((6*(((4*jd-17918)/146097)))/4+1)/2-37);
  temp2 = 10*(((temp1-237)%1461)/4)+5;

  *year = temp1/1461-4712;
  *month =((temp2/306+2)%12)+1;
  *day = (temp2%306)/10+1;
}

double tm2mjd(struct tm date) {
  int m, y, c, x1, x2, x3;
  double dayfrac;

  if (date.tm_mon < 2) {
    m = date.tm_mon+10;
    y = date.tm_mon+1900-1;
  } else {
    m = date.tm_mon-2;
    y = date.tm_year+1900;
  }

  c = y/100;
  y = y-c*100;

  x1 = 146097*c/4;
  x2 = 1461*y/4;
  x3 = (153*m+2)/5;

  dayfrac = ((date.tm_hour*60.0+date.tm_min)*60.0+date.tm_sec)/(60.0*60.0*24.0);

  return(cal2mjd(date.tm_mday, date.tm_mon+1, date.tm_year+1900)+dayfrac);
}

double currentmjd () {
  struct tm *tim;
  struct timeval currenttime;

  gettimeofday(&currenttime, NULL);

  tim = localtime(&currenttime.tv_sec);
  return(tm2mjd(*tim)+(currenttime.tv_usec/1.0e6)/(24.0*60.0*60.0));
}


int turns_to_string(double turns, char type, int dps, int nstr, char str[]) {
  int                 i, hours, sign, whole_turns, dp;
  unsigned long int   revunit, work, scale;
  int                 isec, imin, iunits, right_of_dp;
  char                out[20], text[20], fmt[5];

  /* Record conversion unit and enforce decimal point limits */
  if (type == 'H' || type == 'h') {
    /* Hours mode */
    hours = 1;
    revunit = 86400;
    dp = (dps > 4) ? 4 : dps;
  } else {
    /* Degrees mode */
    hours = 0;
    revunit = 1296000;
    dp = (dps > 3) ? 3 : dps;
  }
  if (dp < 0) dp = 0;
  if (dp != dps) printf("turns_to_string: Invalid number of d.p. requested, "
			"enforcing %d\n", dp);

  /* Record and dispose of sign */
  sign = (turns >= 0) ? 1 : -1;
  turns = turns*(double)sign;

  /* Split into whole number of turns and fractional part. This allows us to
     achieve maximum precision in the seconds part since we can always add
     back a whole number of hours/degrees to the final result without worrying
     about propogation of rounding effects. */
  whole_turns = (int)turns;
  turns -= (double)whole_turns;

  /* Calculate the scaling factor for the conversion */
  i = dp;
  scale = 1.0;
  while (i > 0) {
    scale *= 10;
    i--;
  }
  
  /* Convert - the 0.5 ensures effective rounding, not truncation. */
  work = (unsigned long int)(0.5+(double)(revunit*scale)*turns);

  /* Split off the part to the right of the decimal place */
  right_of_dp = (int)(work - scale*(work/scale));

  /* Convert to seconds and split them off */
  work /= scale;
  isec = (int)(work - 60*(work/60));

  /* Convert to minutes and split them off */
  work /= 60;
  imin = (int)(work - 60*(work/60));

  /* Convert to either hours or degrees and restore whole number of turns */
  iunits = work/60;
  iunits += (hours ? 24 : 360)*whole_turns;

  /* Build the output string */
  out[0] = '\0';
  if (sign == -1) strcat(out, "-");
  sprintf(text, (iunits>99 ? "%d" : "%02d"), iunits);
  strcat(out, text);
  strcat(out ,":");
  sprintf(text, "%02d", imin);
  strcat(out, text);
  strcat(out, ":");
  sprintf(text, "%02d", isec);
  strcat(out, text);
  if (dp > 0) {
    strcat(out, ".");
    sprintf(fmt, "%%0%dd", dp);
    sprintf(text, fmt, right_of_dp);
    strcat(out, text);
  }

  /* Check to see if the output buffer is big enough, if it isn't, make sure
     that the output string is at least null terminated */
  i = strlen(out);
  if (i > nstr) {
    printf("turns_to_string: output string not large enough\n");
    out[nstr-1] = '\0';
    i = nstr-1;
  }
  while (i >= 0) {
    str[i] = out[i];
    i--;
  }

  return(strlen(str));
}

int leap (int year) {
  return (((!(year%4))&&(year%100))||(!(year%400)));
}

static int days[] = {31,28,31,30,31,30,31,31,30,31,30,31};

int cal2dayno(int day, int month, int year) {
  int mon, dayno;

  month--; // For array indexing

  if (leap(year)) {
    days[1] = 29;
  } else {
    days[1] = 28;
  }

  dayno = day;
  for (mon=0; mon<month; mon++) {
    dayno += days[mon];
  }

  return(dayno);
}


void dayno2cal (int dayno, int year, int *day, int *month) {
  int end;

  if (leap(year)) {
    days[1] = 29;
  } else {
    days[1] = 28;
  }

  *month = 0;
  end = days[*month];
  while (dayno>end) {
    (*month)++;
    end+= days[*month];
  }
  end -= days[*month];
  *day = dayno - end;
  (*month)++;

  return;
}

void mjd2dayno(double mjd, int *dayno, int *year, double *ut) {
  int day, month;

  mjd2cal(mjd, &day, &month, year, ut);

  *dayno = cal2dayno(day,month,*year); 
}

static char mjdstr[256];
char* mjd2str(double mjd, int dps) {
  int day, month, year;
  double ut;

  mjd2cal(mjd, &day, &month, &year, &ut);
  turns_to_string(ut, 'H', dps, 256, mjdstr);
  return mjdstr;
}

void my_usleep(u_int64_t usec) {
  double now, till;
  int n;
  /* The current time */
  now = tim();

  /* The time we are sleeping till */
  till = now+usec/1.0e6;

  /* and spin for the rest of the time */
  n = 0;
  while (now<till) {
    now = tim();
    n++;
  }
}
